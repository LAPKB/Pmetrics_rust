use crate::settings::settings;
use extendr_api::List;

use pmcore::prelude::{pharmsol::exa::load::load, simulator::SubjectPredictions, Predictions, *};

use gag::Redirect;
use os_pipe::pipe;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::thread;
use tracing::dispatcher::Dispatch;
use tracing_subscriber;

use crate::print_r_output;

use crate::simulation::SimulationRow;

pub(crate) fn model_parameters<E: Equation>(model_path: PathBuf) -> Vec<String> {
    let (_lib, (_ode, meta)) = unsafe { load::<E>(model_path) };
    meta.get_params().clone()
}

pub(crate) fn simulate<E: Equation>(
    model_path: PathBuf,
    subject: &Subject,
    support_point: &Vec<f64>,
    spp_index: usize,
) -> Result<Vec<SimulationRow>> {
    let (_lib, (model, meta)) = unsafe { load::<E>(model_path) };
    assert!(meta.get_params().len() == support_point.len());
    let predictions: SubjectPredictions = model
        .estimate_predictions(subject, support_point)?
        .get_predictions()
        .into();
    Ok(SimulationRow::from_subject_predictions(
        predictions,
        subject.id(),
        spp_index,
    ))
}

pub(crate) fn fit<E: Equation>(
    model_path: PathBuf,
    data: PathBuf,
    params: List,
    output_path: PathBuf,
) -> std::result::Result<(), anyhow::Error> {
    let (_lib, (eq, meta)) = unsafe { load::<E>(model_path) };
    let settings = settings(params, meta.get_params(), output_path.to_str().unwrap())?;
    let data = data::read_pmetrics(data.to_str().unwrap()).expect("Failed to read data");
    //dbg!(&data);
    let mut algorithm = dispatch_algorithm(settings, eq, data)?;

    // Create a pipe and redirect stdout/stderr to it so we can forward logs to R in
    // real time.
    let (reader, writer) = pipe()?;
    let writer2 = writer.try_clone()?;

    // Redirect stdout and stderr to the pipe writer. Keep the Redirect guards
    // in scope while `.fit()` runs so redirection stays active.
    let _redir_out = Redirect::stdout(writer);
    let _redir_err = Redirect::stderr(writer2);

    // Spawn a thread to read the redirected output and forward it to R as it
    // arrives. Using `extendr_api::rprintln!` ensures the message is delivered
    // to R's console (works on Windows).
    let reader_handle = thread::spawn(move || {
        let mut buf_reader = BufReader::new(reader);
        let mut line = String::new();
        loop {
            line.clear();
            match buf_reader.read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let trimmed = line.trim_end_matches(['\r', '\n'].as_slice());
                    // Forward each captured line to R's console.
                    extendr_api::rprintln!("{}", trimmed);
                }
                Err(e) => {
                    extendr_api::rprintln!("Error reading log pipe: {}", e);
                    break;
                }
            }
        }
    });

    let subscriber = tracing_subscriber::fmt()
        .compact()
        .with_writer(std::io::stdout)
        .finish();
    let dispatch = Dispatch::new(subscriber);

    let fit_result = tracing::dispatcher::with_default(
        &dispatch,
        || -> std::result::Result<(), anyhow::Error> {
            let result = algorithm.fit()?;
            result.write_outputs()?;
            Ok(())
        },
    );

    // Propagate errors from fit
    fit_result?;

    // Drop the redirect guards so the writer side is closed and the reader
    // thread can observe EOF and exit.
    drop(_redir_out);
    drop(_redir_err);

    // Wait for the reader thread to finish forwarding remaining logs.
    if let Err(e) = reader_handle.join() {
        extendr_api::rprintln!("Log capture thread panicked: {:?}", e);
    }

    Ok(())
}
