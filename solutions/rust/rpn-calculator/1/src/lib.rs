#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

fn extract_ops(stack: &mut Vec<i32>) -> Result<(i32, i32), String> {
    let op2 = stack
        .pop()
        .ok_or(format!("missing second operand"))?;
    let op1 = stack
        .pop()
        .ok_or(format!("missing first operand"))?;

    Ok((op1, op2))
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack: Vec<i32> = vec![];
    if inputs.len() == 0 {
        return None;
    }
    for i in inputs {
        match i {
            CalculatorInput::Add => {
                let Ok((v1, v2)) = extract_ops(&mut stack) else { return None };
                stack.push(v1 + v2)
            },
            CalculatorInput::Subtract => {
                let Ok((v1, v2)) = extract_ops(&mut stack) else { return None };
                stack.push(v1 - v2)
            },
            CalculatorInput::Multiply => {
                let Ok((v1, v2)) = extract_ops(&mut stack) else { return None };
                stack.push(v1 * v2)
            },
            CalculatorInput::Divide => {
                let Ok((v1, v2)) = extract_ops(&mut stack) else { return None };
                stack.push(v1 / v2)
            },
            CalculatorInput::Value(v) => {
                stack.push(*v)
            },
        };
    };
    if stack.len() != 1 {
        return None;
    }
    stack.pop()
}
