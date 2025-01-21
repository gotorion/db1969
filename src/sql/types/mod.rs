use super::parser::ast::{Consts, Expression};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DataType {
    Boolean,
    Integer,
    Float,
    String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Value {
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
}

impl Value {
    pub fn from_expression(expr: Expression) -> Self {
        match expr {
            Expression::Consts(Consts::Null) => Self::Null,
            Expression::Consts(Consts::Boolean(b)) => Self::Boolean(b),
            Expression::Consts(Consts::Integer(i)) => Self::Integer(i),
            Expression::Consts(Consts::Float(f)) => Self::Float(f),
            Expression::Consts(Consts::String(s)) => Self::String(s),
        }
    }

    pub fn data_type(&self) -> Option<DataType> {
        match self {
            Self::Null => None,
            Self::Boolean(_) => Some(DataType::Boolean),
            Self::Integer(_) => Some(DataType::Integer),
            Self::Float(_) => Some(DataType::Float),
            Self::String(_) => Some(DataType::String),
        }
    }
}

pub type Row = Vec<Value>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_expression() -> () {
        {
            let expr = Expression::Consts(Consts::Integer(42));
            let value = Value::from_expression(expr);
            assert_eq!(value, Value::Integer(42));
        }
        {
            let expr = Expression::Consts(Consts::String("hello".to_string()));
            let value = Value::from_expression(expr);
            assert_eq!(value, Value::String(String::from("hello")));
        }
    }

    #[test]
    fn test_data_type() -> () {
        {
            let value = Value::Integer(42);
            assert_eq!(value.data_type(), Some(DataType::Integer));
        }
        {
            let value = Value::String("hello".to_string());
            assert_eq!(value.data_type(), Some(DataType::String));
        }
    }
}
