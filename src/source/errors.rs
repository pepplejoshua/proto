#![allow(dead_code)]

use super::source::SourceRef;

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum LexError {
    InvalidCharacter(SourceRef),
    CannotMakeSignedNumber(SourceRef),
    CannotMakeUnsignedNumber(SourceRef),
    EmptyCharacterLiteral(SourceRef),
    UnterminatedCharacterLiteral(SourceRef),
    UnterminatedStringLiteral(SourceRef),
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum ParseError {
    Expected(String, SourceRef, Option<String>),
    ConstantDeclarationNeedsTypeOrInitValue(SourceRef),
    CannotParseAnExpression(SourceRef),
    CannotParseAType(SourceRef),
    MalformedDeclaration(String, SourceRef),
    NoVariableAtCurrentScope(SourceRef),
    NoCodeBlockAllowedInCurrentContext(SourceRef),
    NoLoopAtTopLevel(SourceRef),
    NoBreakOutsideLoop(SourceRef),
    NoContinueOutsideLoop(SourceRef),
    MisuseOfPubKeyword(SourceRef),
    UnterminatedCodeBlock(SourceRef, Option<String>),
    ReturnInstructionOutsideFunction(SourceRef),
    CyclicalDependencyBetweenNodes { cycle: String, src: SourceRef },
    TooManyErrors(SourceRef),
}

impl ParseError {
    pub fn get_error_src(&self) -> SourceRef {
        match self {
            ParseError::Expected(_, src, _)
            | ParseError::ConstantDeclarationNeedsTypeOrInitValue(src)
            | ParseError::CannotParseAnExpression(src)
            | ParseError::CannotParseAType(src)
            | ParseError::MalformedDeclaration(_, src)
            | ParseError::NoCodeBlockAllowedInCurrentContext(src)
            | ParseError::NoLoopAtTopLevel(src)
            | ParseError::NoBreakOutsideLoop(src)
            | ParseError::NoContinueOutsideLoop(src)
            | ParseError::MisuseOfPubKeyword(src)
            | ParseError::UnterminatedCodeBlock(src, _)
            | ParseError::ReturnInstructionOutsideFunction(src)
            | ParseError::TooManyErrors(src)
            | ParseError::CyclicalDependencyBetweenNodes { src, .. }
            | ParseError::NoVariableAtCurrentScope(src) => src.clone(),
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub struct ParseWarning {
    pub msg: String,
    pub src: SourceRef,
}

impl ParseWarning {
    pub fn get_warning_src(&self) -> SourceRef {
        self.src.clone()
    }
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum CheckerError {
    TypeMismatch {
        loc: SourceRef,
        expected: String,
        found: String,
    },
    NumberTypeDefaultInferenceFailed {
        loc: SourceRef,
        number: String,
    },
    NumberTypeInferenceFailed {
        loc: SourceRef,
        number: String,
        given_type: String,
    },
    ReferenceToUndefinedName {
        loc: SourceRef,
        var_name: String,
    },
    InvalidUseOfBinaryOperator {
        loc: SourceRef,
        op: String,
        left: String,
        right: String,
    },
    InvalidUseOfUnaryOperator {
        loc: SourceRef,
        op: String,
        operand: String,
        tip: Option<String>,
    },
    InvalidType {
        loc: SourceRef,
        type_name: String,
    },
    IncompleteType {
        loc: SourceRef,
        type_name: String,
    },
    // TODO: track the location of the previous definition
    NameAlreadyDefined {
        loc: SourceRef,
        name: String,
    },
    UseOfUninitializedVariable {
        loc: SourceRef,
        name: String,
    },
    UseOfErroredVariableOrConstant {
        is_const: bool,
        loc: SourceRef,
        name: String,
    },
    NameIsNotCallable {
        name: String,
        name_ty: String,
        loc: SourceRef,
    },
    MismatchingReturnType {
        exp: String,
        given: String,
        loc_given: SourceRef,
    },
    IncorrectFunctionArity {
        func: String,
        exp: usize,
        given: usize,
        loc_given: SourceRef,
    },
    CannotInferTypeOfEmptyArray {
        loc: SourceRef,
    },
    MismatchingStaticArrayItemTypes {
        expected_ty: String,
        given_ty: String,
        loc: SourceRef,
    },
    StaticArrayTypeInferenceFailed {
        given_ty: String,
        arr_loc: SourceRef,
    },
    OptionalTypeInferenceFailed {
        given_ty: String,
        opt_loc: SourceRef,
    },
    OptionalTypeInferenceFailedWithoutContextualTy {
        opt_loc: SourceRef,
    },
    NonConstantNumberSizeForStaticArray {
        loc: SourceRef,
    },
    MismismatchStaticArrayLength {
        exp: String,
        given: String,
        arr_loc: SourceRef,
    },
    ConditionShouldBeTypedBool {
        given_ty: String,
        loc: SourceRef,
    },
    ExpectedArrayOrSlice {
        given_ty: String,
        loc: SourceRef,
    },
    PrintRequiresAStringArg {
        is_println: bool,
        given_ty: String,
        loc: SourceRef,
    },
    IndexIntoOpRequiresArraySliceOrString {
        given_ty: String,
        loc: SourceRef,
    },
    Expected(String, SourceRef, Option<String>),
    AccessMemberOpCannotBePerformedOnType {
        given_ty: String,
        loc: SourceRef,
    },
    MemberDoesNotExist {
        given_ty: String,
        mem: String,
        loc: SourceRef,
    },
    StructHasNoInitFunction {
        given_ty: String,
        loc: SourceRef,
    },
    CannotAssignToTarget {
        target: String,
        loc: SourceRef,
    },
    CannotAssignToImmutableTarget {
        target: String,
        loc: SourceRef,
    },
    TooManyErrors,
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum CodeGenError {
    CompileErr(),
}
