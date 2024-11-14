#![allow(dead_code)]

use std::rc::Rc;

use super::source::SourceRef;

#[derive(Debug, Clone)]
pub enum LexError {
    InvalidCharacter(Rc<SourceRef>),
    CannotMakeSignedNumber(Rc<SourceRef>),
    CannotMakeUnsignedNumber(Rc<SourceRef>),
    EmptyCharacterLiteral(Rc<SourceRef>),
    UnterminatedCharacterLiteral(Rc<SourceRef>),
    UnterminatedStringLiteral(Rc<SourceRef>),
    DecimalLiteralWithMultipleDecimalPoints(Rc<SourceRef>),
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Expected(String, Rc<SourceRef>, Option<String>),
    ConstantDeclarationNeedsTypeOrInitValue(Rc<SourceRef>),
    CannotParseAnExpression(Rc<SourceRef>),
    MalformedDeclaration(String, Rc<SourceRef>),
    ReusedOfIdentifier(Rc<SourceRef>),
    UnterminatedCodeBlock(Rc<SourceRef>, Option<String>),
    MalformedPubDeclaration { src: Rc<SourceRef> },
    CyclicalDependencyBetweenNodes { cycle: String, src: Rc<SourceRef> },
    ParsedInstructionIsNotAllowedAtThisLevel { level: String, src: Rc<SourceRef> },
    TooManyErrors(Rc<SourceRef>),
}

#[derive(Debug, Clone)]
pub enum SemanError {
    NoMainFunctionProvided {
        filename: Rc<String>,
    },
    TypeMismatch {
        loc: Rc<SourceRef>,
        expected: String,
        found: String,
    },
    IntegerTypeDefaultInferenceFailed {
        loc: Rc<SourceRef>,
        number: String,
    },
    FloatTypeDefaultInferenceFailed {
        loc: Rc<SourceRef>,
        number: String,
    },
    IntegerTypeCheckFailed {
        loc: Rc<SourceRef>,
        number: String,
        given_type: String,
    },
    FloatTypeCheckFailed {
        loc: Rc<SourceRef>,
        number: String,
        given_type: String,
    },
    ReferenceToUndefinedName {
        loc: Rc<SourceRef>,
        var_name: String,
    },
    InvalidUseOfBinaryOperator {
        loc: Rc<SourceRef>,
        op: String,
        left: String,
        right: String,
    },
    InvalidUseOfUnaryOperator {
        loc: Rc<SourceRef>,
        op: String,
        operand: String,
        tip: Option<String>,
    },
    InvalidType {
        loc: Rc<SourceRef>,
        type_name: String,
    },
    IncompleteType {
        loc: Rc<SourceRef>,
        type_name: String,
    },
    // TODO: track the location of the previous definition
    NameAlreadyDefined {
        loc: Rc<SourceRef>,
        name: String,
    },
    UseOfUninitializedVariable {
        loc: Rc<SourceRef>,
        name: String,
    },
    UseOfErroredVariableOrConstant {
        is_const: bool,
        loc: Rc<SourceRef>,
        name: String,
    },
    ExpectedFunctionType {
        found: String,
        loc: Rc<SourceRef>,
    },
    MismatchingReturnType {
        exp: String,
        given: String,
        loc_given: Rc<SourceRef>,
    },
    IncorrectFunctionArity {
        expected: usize,
        given: usize,
        loc: Rc<SourceRef>,
    },
    CannotInferTypeOfEmptyArray {
        loc: Rc<SourceRef>,
    },
    MismatchingStaticArrayItemTypes {
        expected_ty: String,
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    StaticArrayTypeCheckFailed {
        given_ty: String,
        arr_loc: Rc<SourceRef>,
    },
    OptionalTypeInferenceFailed {
        given_ty: String,
        opt_loc: Rc<SourceRef>,
    },
    OptionalTypeInferenceFailedWithoutContextualTy {
        opt_loc: Rc<SourceRef>,
    },
    NonConstantNumberSizeForStaticArray {
        loc: Rc<SourceRef>,
    },
    MismismatchStaticArrayLength {
        exp: String,
        given: String,
        arr_loc: Rc<SourceRef>,
    },
    ExpectedArrayOrSlice {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    PrintRequiresAStringArg {
        is_println: bool,
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    IndexIntoOpRequiresArraySliceOrString {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    Expected(String, Rc<SourceRef>, Option<String>),
    AccessMemberOpCannotBePerformedOnType {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    MemberDoesNotExist {
        given_ty: String,
        mem: String,
        loc: Rc<SourceRef>,
    },
    CannotAssignToTarget {
        loc: Rc<SourceRef>,
    },
    CannotAssignToImmutableTarget {
        target: String,
        loc: Rc<SourceRef>,
    },
    CannotAccessNonConstFuncOnConstTarget {
        loc: Rc<SourceRef>,
    },
    CannotReturnFromInsideADeferIns {
        loc: Rc<SourceRef>,
    },
    FunctionInDeferShouldReturnVoid {
        loc: Rc<SourceRef>,
    },
    TooManyErrors,
}
