use crate::settings::settings;
use extendr_api::prelude::*;
use pmcore::bestdose::{BestDoseProblem, BestDoseResult, DoseRange, Target};
use pmcore::prelude::{data, ODE};
use pmcore::routines::initialization::parse_prior;
use std::path::PathBuf;

/// Helper to parse target type from string
pub(crate) fn parse_target_type(target_str: &str) -> std::result::Result<Target, String> {
    match target_str.to_lowercase().as_str() {
        "concentration" => Ok(Target::Concentration),
        "auc_from_zero" | "auc" => Ok(Target::AUCFromZero),
        "auc_from_last_dose" | "auc_interval" => Ok(Target::AUCFromLastDose),
        _ => Err(format!(
            "Invalid target type: {}. Must be 'concentration', 'auc_from_zero', or 'auc_from_last_dose'",
            target_str
        )),
    }
}

/// R-compatible prediction row for BestDose output
#[derive(Debug, IntoDataFrameRow)]
pub struct BestDosePredictionRow {
    id: String,
    time: f64,
    observed: f64,
    pop_mean: f64,
    pop_median: f64,
    post_mean: f64,
    post_median: f64,
    outeq: usize,
}

impl BestDosePredictionRow {
    pub fn from_np_prediction(
        pred: &pmcore::routines::output::predictions::NPPredictionRow,
        id: &str,
    ) -> Self {
        Self {
            id: id.to_string(),
            time: pred.time(),
            observed: pred.obs().unwrap_or(0.0),
            pop_mean: pred.pop_mean(),
            pop_median: pred.pop_median(),
            post_mean: pred.post_mean(),
            post_median: pred.post_median(),
            outeq: pred.outeq(),
        }
    }
}

/// R-compatible AUC prediction row
#[derive(Debug, IntoDataFrameRow)]
pub struct BestDoseAucRow {
    time: f64,
    auc: f64,
}

/// Convert BestDoseResult to R-compatible list structure
pub(crate) fn convert_bestdose_result_to_r(
    result: BestDoseResult,
) -> std::result::Result<Robj, String> {
    // Extract doses
    let doses: Vec<f64> = result.doses();

    // Objective function
    let objf = result.objf();

    // Status
    let status_str = format!("{:?}", result.status());

    // Predictions as data frame
    let pred_rows: Vec<BestDosePredictionRow> = result
        .predictions()
        .predictions()
        .iter()
        .map(|p| BestDosePredictionRow::from_np_prediction(p, "subject_1"))
        .collect();
    let pred_df = pred_rows
        .into_dataframe()
        .map_err(|e| format!("Failed to create predictions dataframe: {:?}", e))?;

    // AUC predictions (if available)
    let auc_val = if let Some(auc_preds) = result.auc_predictions() {
        let auc_rows: Vec<BestDoseAucRow> = auc_preds
            .iter()
            .map(|(time, auc)| BestDoseAucRow {
                time: *time,
                auc: *auc,
            })
            .collect();
        let auc_df = auc_rows
            .into_dataframe()
            .map_err(|e| format!("Failed to create AUC dataframe: {:?}", e))?;
        Robj::from(auc_df)
    } else {
        Robj::from(()) // NULL for no AUC
    };

    // Optimization method
    let method_str = format!("{}", result.optimization_method());

    // Build the list using list! macro
    let output = list!(
        doses = doses,
        objf = objf,
        status = status_str,
        predictions = pred_df,
        auc_predictions = auc_val,
        method = method_str
    );

    Ok(output.into())
}

/// Execute bestdose optimization for ODE models
pub(crate) fn bestdose_ode(
    model_path: PathBuf,
    prior_path: PathBuf,
    past_data_path: Option<PathBuf>,
    target_data_path: PathBuf,
    time_offset: Option<f64>,
    dose_min: f64,
    dose_max: f64,
    bias_weight: f64,
    target_type: &str,
    params: List,
) -> std::result::Result<BestDoseResult, String> {
    // 1. Load the model
    let (_lib, (eq, meta)) =
        unsafe { pmcore::prelude::pharmsol::exa::load::load::<ODE>(model_path) };

    // 2. Parse settings from R list
    let settings = settings(params, meta.get_params(), "/tmp/bestdose")
        .map_err(|e| format!("Failed to parse settings: {}", e))?;

    // 3. Parse the prior (theta + weights)
    let (population_theta, prior_weights) = parse_prior(
        &prior_path.to_str().unwrap().to_string(),
        &settings,
    )
    .map_err(|e| format!("Failed to parse prior: {}", e))?;
    
    let population_weights = prior_weights.ok_or_else(|| {
        "Prior file must contain a 'prob' column with weights".to_string()
    })?;

    // 4. Load past data (if provided)
    let past_data = if let Some(path) = past_data_path {
        let data = data::read_pmetrics(path.to_str().unwrap())
            .map_err(|e| format!("Failed to read past data: {}", e))?;
        let subjects = data.subjects();
        if subjects.is_empty() {
            return Err("Past data file contains no subjects".to_string());
        }
        Some(subjects[0].clone())
    } else {
        None
    };

    // 5. Load target data
    let target_data = {
        let data = data::read_pmetrics(target_data_path.to_str().unwrap())
            .map_err(|e| format!("Failed to read target data: {}", e))?;
        let subjects = data.subjects();
        if subjects.is_empty() {
            return Err("Target data file contains no subjects".to_string());
        }
        subjects[0].clone()
    };

    // 6. Parse target type
    let target_enum = parse_target_type(target_type)?;

    // 7. Create dose range
    let doserange = DoseRange::new(dose_min, dose_max);

    // 8. Create and solve bestdose problem
    let problem = BestDoseProblem::new(
        &population_theta,
        &population_weights,
        past_data,
        target_data,
        time_offset,
        eq,
        doserange,
        bias_weight,
        settings,
        target_enum,
    )
    .map_err(|e| format!("Failed to create BestDose problem: {}", e))?;

    let result = problem
        .optimize()
        .map_err(|e| format!("Optimization failed: {}", e))?;

    Ok(result)
}

/// Execute bestdose optimization for analytical models (placeholder - not yet supported)
pub(crate) fn bestdose_analytical(
    _model_path: PathBuf,
    _prior_path: PathBuf,
    _past_data_path: Option<PathBuf>,
    _target_data_path: PathBuf,
    _time_offset: Option<f64>,
    _dose_min: f64,
    _dose_max: f64,
    _bias_weight: f64,
    _target_type: &str,
    _params: List,
) -> std::result::Result<BestDoseResult, String> {
    Err("BestDose for analytical models is not yet supported".to_string())
}
