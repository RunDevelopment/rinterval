use std::{collections::HashSet, path::PathBuf};

use rinterval::{ArithResult, IInterval};

fn snapshot_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("snapshots")
}

pub fn match_snapshot(name: &str, content: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut filename = name
        .to_string()
        .replace(|c: char| !(c.is_alphanumeric() || c == '_'), " ")
        .replace("  ", " ")
        .replace("  ", " ");
    filename = filename
        .strip_prefix("Arithmetic ")
        .unwrap_or(&filename)
        .to_string();

    let snapshot_path = snapshot_dir().join(filename + ".yml");

    if !snapshot_path.exists() {
        std::fs::create_dir_all(snapshot_path.parent().unwrap())?;
        std::fs::write(&snapshot_path, content)?;
    } else {
        let existing_content = std::fs::read_to_string(&snapshot_path)?.replace("\r\n", "\n");
        if existing_content != content {
            std::fs::write(&snapshot_path, content)?;
            return Err("Snapshot mismatch".into());
        }
    }
    Ok(())
}

pub fn assert_snapshot(name: &str, content: &str) {
    if let Err(e) = match_snapshot(name, content) {
        panic!("Snapshot assertion failed for {name}: {e}");
    }
}

fn get_interval_types<'a>(ranges: impl Iterator<Item = &'a IInterval>) -> (usize, String) {
    let unique_types: HashSet<_> = ranges.map(|r| r.ty).collect();
    if unique_types.is_empty() {
        return (0, "none".to_string());
    }
    let mut unique_types = Vec::from_iter(unique_types);
    unique_types.sort();

    let formattted = unique_types
        .iter()
        .map(|ty| format!("{ty:?}"))
        .collect::<Vec<_>>()
        .join(" | ");

    (unique_types.len(), formattted)
}
pub fn format_unary_data(data: &[(IInterval, ArithResult<IInterval>)]) -> String {
    let input_types = get_interval_types(data.iter().map(|(i, _)| i));
    let mut result_types = get_interval_types(data.iter().filter_map(|(_, r)| r.as_ref().ok()));
    let has_error = data.iter().any(|(_, res)| res.is_err());
    if has_error {
        result_types.1 += " | Error";
    }

    let mut out = String::new();
    out.push_str(&format!("{} => {}: >\n", input_types.1, result_types.1));

    for (input, res) in data {
        out.push_str("  "); // indent

        if input_types.0 == 1 {
            out.push_str(&input.to_string_untyped());
        } else {
            out.push_str(&input.to_string());
        }

        out.push_str("  ->  ");

        let range = match res {
            Ok(range) => {
                if result_types.0 == 1 {
                    range.to_string_untyped()
                } else {
                    range.to_string()
                }
            }
            Err(err) => format!("Error: {err:?}"),
        };
        out.push_str(&range);
        out.push('\n');
    }

    out
}
pub fn format_binary_data(data: &[(IInterval, IInterval, ArithResult<IInterval>)]) -> String {
    let lhs_types = get_interval_types(data.iter().map(|(lhs, _, _)| lhs));
    let rhs_types = get_interval_types(data.iter().map(|(_, rhs, _)| rhs));
    let mut result_types = get_interval_types(data.iter().filter_map(|(_, _, r)| r.as_ref().ok()));
    let has_error = data.iter().any(|(_, _, res)| res.is_err());
    if has_error {
        result_types.1 += " | Error";
    }

    let mut out = String::new();
    out.push_str(&format!(
        "{} x {} => {}: >\n",
        lhs_types.1, rhs_types.1, result_types.1
    ));

    for (lhs, rhs, res) in data {
        out.push_str("  "); // indent

        if lhs_types.0 == 1 && rhs_types.0 == 1 {
            out.push_str(&format!(
                "{}  .  {}",
                lhs.to_string_untyped(),
                rhs.to_string_untyped()
            ));
        } else {
            out.push_str(&format!("{lhs}  .  {rhs}"));
        }

        out.push_str("  ->  ");

        let range = match res {
            Ok(range) => {
                if result_types.0 == 1 {
                    range.to_string_untyped()
                } else {
                    range.to_string()
                }
            }
            Err(err) => format!("Error: {err:?}"),
        };
        out.push_str(&range);
        out.push('\n');
    }

    out
}
