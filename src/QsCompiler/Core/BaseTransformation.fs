﻿// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

namespace Microsoft.Quantum.QsCompiler.Transformations.Core

open System.Collections.Immutable
open System.Linq
open System.Numerics
open Microsoft.Quantum.QsCompiler.DataTypes
open Microsoft.Quantum.QsCompiler.SyntaxExtensions
open Microsoft.Quantum.QsCompiler.SyntaxTokens
open Microsoft.Quantum.QsCompiler.SyntaxTree

type QsArgumentTuple = QsTuple<LocalVariableDeclaration<QsLocalSymbol>>
type private ExpressionKind = QsExpressionKind<TypedExpression,Identifier,ResolvedType>
type private ExpressionType = QsTypeKind<ResolvedType, UserDefinedType, QsTypeParameter, CallableInformation>

type MyHandler<'T> (handler : 'T -> 'T) = member this.Call = handler

type MyEvent<'T,'F> (defaultHandler : 'T -> 'T, final : 'T -> 'F) =
    
    let removeAt i lst =
        List.append
        <| List.take i lst
        <| List.skip (i+1) lst

    member val private Handlers : MyHandler<'T> list = [] with get, set
    member val private CombinedHandleValid = true with get, set
    member val private CombinedHandle = defaultHandler with get, set

    member private this.UpdateCombinedHandle () =
        this.CombinedHandle <- match this.Handlers with
                               | [f] -> f.Call
                               | f1 :: fn -> List.fold (>>) f1.Call <| List.map (fun (f : MyHandler<'T>) -> f.Call) fn
                               | [] -> defaultHandler
        this.CombinedHandleValid <- true

    member this.Add handler =
        match this.Handlers with
        | [] -> this.Handlers <- [handler]
                this.CombinedHandle <- handler.Call
                this.CombinedHandleValid <- true
        | _ -> this.Handlers <- this.Handlers @ [handler]
               if this.CombinedHandleValid then
                   this.CombinedHandle <- this.CombinedHandle >> handler.Call
               else
                   this.UpdateCombinedHandle()

    member this.Add (handler : 'T -> 'T) =
        System.Func<'T,'T> handler |> this.Add
    
    member this.Add (handler : System.Func<'T, 'T>) =
        let rtrn = MyHandler handler.Invoke
        this.Add rtrn
        rtrn

    member this.Remove handler =
        this.Handlers <- match Seq.tryFindIndexBack (fun x -> x = handler) this.Handlers with
                         | Some i -> removeAt i this.Handlers
                         | None -> this.Handlers
        this.CombinedHandleValid <- false

    member this.Call arg =
        if not this.CombinedHandleValid then this.UpdateCombinedHandle()
        this.CombinedHandle arg |> final

    member this.CallDefault = defaultHandler

type MyEvent<'T> (defaultHandler : 'T -> 'T) = inherit MyEvent<'T,'T>(defaultHandler, id)

/// Convention:
/// All methods starting with "on" implement the transformation syntax tree element.
/// All methods starting with "before" group a set of elements, and are called before applying the transformation
/// even if the corresponding transformation routine (starting with "on") is overridden.
type BaseTransformation() as this =

    (*Expression Kind*)

    member val beforeCallLike = MyEvent<TypedExpression * TypedExpression> this.beforeCallLikeDefault
    member this.beforeCallLikeDefault (method, arg) = (method, arg)

    member val beforeFunctorApplication = MyEvent<TypedExpression> this.beforeFunctorApplicationDefault
    member this.beforeFunctorApplicationDefault ex = ex

    member val beforeModifierApplication = MyEvent<TypedExpression> this.beforeModifierApplicationDefault
    member this.beforeModifierApplicationDefault ex = ex

    member val beforeBinaryOperatorExpression = MyEvent<TypedExpression * TypedExpression> this.beforeBinaryOperatorExpressionDefault
    member this.beforeBinaryOperatorExpressionDefault (lhs, rhs) = (lhs, rhs)

    member val beforeUnaryOperatorExpression = MyEvent<TypedExpression> this.beforeUnaryOperatorExpressionDefault
    member this.beforeUnaryOperatorExpressionDefault ex = ex


    member val onTypedExpression = MyEvent<TypedExpression> this.onTypedExpressionDefault
    member val onResolvedType = MyEvent<ResolvedType> this.onResolvedTypeDefault

    member val onIdentifier = MyEvent<Identifier * QsNullable<ImmutableArray<ResolvedType>>, ExpressionKind> (this.onIdentifierDefault, Identifier)
    member this.onIdentifierDefault (sym, tArgs) = (sym, tArgs |> QsNullable<_>.Map (fun ts -> (ts |> Seq.map this.onResolvedType.Call).ToImmutableArray()))

    member val onOperationCall = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onOperationCallDefault, CallLikeExpression)
    member this.onOperationCallDefault (method, arg) = (this.onTypedExpression.Call method, this.onTypedExpression.Call arg)

    member val onFunctionCall = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onFunctionCallDefault, CallLikeExpression)
    member this.onFunctionCallDefault (method, arg) = (this.onTypedExpression.Call method, this.onTypedExpression.Call arg)

    member val onPartialApplication = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onPartialApplicationDefault, CallLikeExpression)
    member this.onPartialApplicationDefault (method, arg) = (this.onTypedExpression.Call method, this.onTypedExpression.Call arg)

    member val onAdjointApplication = MyEvent<TypedExpression, ExpressionKind> (this.onAdjointApplicationDefault, AdjointApplication)
    member this.onAdjointApplicationDefault ex = this.onTypedExpression.Call ex

    member val onControlledApplication = MyEvent<TypedExpression, ExpressionKind> (this.onControlledApplicationDefault, ControlledApplication)
    member this.onControlledApplicationDefault ex = this.onTypedExpression.Call ex

    member val onUnwrapApplication = MyEvent<TypedExpression, ExpressionKind> (this.onUnwrapApplicationDefault, UnwrapApplication)
    member this.onUnwrapApplicationDefault ex = this.onTypedExpression.Call ex

    member val onUnitValue = MyEvent<unit, ExpressionKind> (this.onUnitValueDefault, (fun () -> ExpressionKind.UnitValue))
    member this.onUnitValueDefault () = ()

    member val onMissingExpression = MyEvent<unit, ExpressionKind> (this.onMissingExpressionDefault, (fun () -> MissingExpr))
    member this.onMissingExpressionDefault () = ()

    member val onInvalidExpression = MyEvent<unit, ExpressionKind> (this.onInvalidExpressionDefault, (fun () -> InvalidExpr))
    member this.onInvalidExpressionDefault () = ()

    member val onValueTuple = MyEvent<ImmutableArray<TypedExpression>, ExpressionKind> (this.onValueTupleDefault, ValueTuple)
    member this.onValueTupleDefault vs = (vs |> Seq.map this.onTypedExpression.Call).ToImmutableArray()

    member val onArrayItem = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onArrayItemDefault, ArrayItem)
    member this.onArrayItemDefault (arr, idx) = (this.onTypedExpression.Call arr, this.onTypedExpression.Call idx)

    member val onNamedItem = MyEvent<TypedExpression * Identifier, ExpressionKind> (this.onNamedItemDefault, NamedItem)
    member this.onNamedItemDefault (ex, acc) = (this.onTypedExpression.Call ex, acc)

    member val onValueArray = MyEvent<ImmutableArray<TypedExpression>, ExpressionKind> (this.onValueArrayDefault, ValueArray)
    member this.onValueArrayDefault vs = (vs |> Seq.map this.onTypedExpression.Call).ToImmutableArray()

    member val onNewArray = MyEvent<ResolvedType * TypedExpression, ExpressionKind> (this.onNewArrayDefault, NewArray)
    member this.onNewArrayDefault (bt, idx) = (this.onResolvedType.Call bt, this.onTypedExpression.Call idx)

    member val onIntLiteral = MyEvent<int64, ExpressionKind> (this.onIntLiteralDefault, IntLiteral)
    member this.onIntLiteralDefault i = i

    member val onBigIntLiteral = MyEvent<BigInteger, ExpressionKind> (this.onBigIntLiteralDefault, BigIntLiteral)
    member this.onBigIntLiteralDefault b = b

    member val onDoubleLiteral = MyEvent<double, ExpressionKind> (this.onDoubleLiteralDefault, DoubleLiteral)
    member this.onDoubleLiteralDefault d = d

    member val onBoolLiteral = MyEvent<bool, ExpressionKind> (this.onBoolLiteralDefault, BoolLiteral)
    member this.onBoolLiteralDefault b = b

    member val onResultLiteral = MyEvent<QsResult, ExpressionKind> (this.onResultLiteralDefault, ResultLiteral)
    member this.onResultLiteralDefault r = r

    member val onPauliLiteral = MyEvent<QsPauli, ExpressionKind> (this.onPauliLiteralDefault, PauliLiteral)
    member this.onPauliLiteralDefault p = p

    member val onStringLiteral = MyEvent<NonNullable<string> * ImmutableArray<TypedExpression>, ExpressionKind> (this.onStringLiteralDefault, StringLiteral)
    member this.onStringLiteralDefault (s, exs) = (s, (exs |> Seq.map this.onTypedExpression.Call).ToImmutableArray())

    member val onRangeLiteral = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onRangeLiteralDefault, RangeLiteral)
    member this.onRangeLiteralDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onCopyAndUpdateExpression = MyEvent<TypedExpression * TypedExpression * TypedExpression, ExpressionKind> (this.onCopyAndUpdateExpressionDefault, CopyAndUpdate)
    member this.onCopyAndUpdateExpressionDefault (lhs, accEx, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call accEx, this.onTypedExpression.Call rhs)

    member val onConditionalExpression = MyEvent<TypedExpression * TypedExpression * TypedExpression, ExpressionKind> (this.onConditionalExpressionDefault, CONDITIONAL)
    member this.onConditionalExpressionDefault (cond, ifTrue, ifFalse) = (this.onTypedExpression.Call cond, this.onTypedExpression.Call ifTrue, this.onTypedExpression.Call ifFalse)

    member val onEquality = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onEqualityDefault, EQ)
    member this.onEqualityDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onInequality = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onInequalityDefault, NEQ)
    member this.onInequalityDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onLessThan = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onLessThanDefault, LT)
    member this.onLessThanDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onLessThanOrEqual = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onLessThanOrEqualDefault, LTE)
    member this.onLessThanOrEqualDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onGreaterThan = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onGreaterThanDefault, GT)
    member this.onGreaterThanDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onGreaterThanOrEqual = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onGreaterThanOrEqualDefault, GTE)
    member this.onGreaterThanOrEqualDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onLogicalAnd = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onLogicalAndDefault, AND)
    member this.onLogicalAndDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onLogicalOr = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onLogicalOrDefault, OR)
    member this.onLogicalOrDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onAddition = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onAdditionDefault, ADD)
    member this.onAdditionDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onSubtraction = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onSubtractionDefault, SUB)
    member this.onSubtractionDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onMultiplication = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onMultiplicationDefault, MUL)
    member this.onMultiplicationDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onDivision = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onDivisionDefault, DIV)
    member this.onDivisionDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onExponentiate = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onExponentiateDefault, POW)
    member this.onExponentiateDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onModulo = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onModuloDefault, MOD)
    member this.onModuloDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onLeftShift = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onLeftShiftDefault, LSHIFT)
    member this.onLeftShiftDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onRightShift = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onRightShiftDefault, RSHIFT)
    member this.onRightShiftDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onBitwiseExclusiveOr = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onBitwiseExclusiveOrDefault, BXOR)
    member this.onBitwiseExclusiveOrDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onBitwiseOr = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onBitwiseOrDefault, BOR)
    member this.onBitwiseOrDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onBitwiseAnd = MyEvent<TypedExpression * TypedExpression, ExpressionKind> (this.onBitwiseAndDefault, BAND)
    member this.onBitwiseAndDefault (lhs, rhs) = (this.onTypedExpression.Call lhs, this.onTypedExpression.Call rhs)

    member val onLogicalNot = MyEvent<TypedExpression, ExpressionKind> (this.onLogicalNotDefault, NOT)
    member this.onLogicalNotDefault ex = this.onTypedExpression.Call ex

    member val onNegative = MyEvent<TypedExpression, ExpressionKind> (this.onNegativeDefault, NEG)
    member this.onNegativeDefault ex = this.onTypedExpression.Call ex

    member val onBitwiseNot = MyEvent<TypedExpression, ExpressionKind> (this.onBitwiseNotDefault, BNOT)
    member this.onBitwiseNotDefault ex = this.onTypedExpression.Call ex


    member private this.dispatchCallLikeExpression (method, arg) =
        match method.ResolvedType.Resolution with
            | _ when TypedExpression.IsPartialApplication (CallLikeExpression (method, arg)) -> this.onPartialApplication.Call (method, arg)
            | ExpressionType.Operation _                                                     -> this.onOperationCall.Call (method, arg)
            | _                                                                              -> this.onFunctionCall.Call (method, arg)

    member val onExpressionKind = MyEvent<ExpressionKind> this.onExpressionKindDefault
    member this.onExpressionKindDefault kind =
        match kind with
        | Identifier (sym, tArgs)                          -> this.onIdentifier.Call                 (sym, tArgs)
        | CallLikeExpression (method,arg)                  -> this.dispatchCallLikeExpression        ((method, arg)        |> this.beforeCallLike.Call)
        | AdjointApplication ex                            -> this.onAdjointApplication.Call         (ex                   |> (this.beforeFunctorApplication.Call >> this.beforeModifierApplication.Call))
        | ControlledApplication ex                         -> this.onControlledApplication.Call      (ex                   |> (this.beforeFunctorApplication.Call >> this.beforeModifierApplication.Call))
        | UnwrapApplication ex                             -> this.onUnwrapApplication.Call          (ex                   |> this.beforeModifierApplication.Call)
        | UnitValue                                        -> this.onUnitValue.Call                  ()
        | MissingExpr                                      -> this.onMissingExpression.Call          ()
        | InvalidExpr                                      -> this.onInvalidExpression.Call          ()
        | ValueTuple vs                                    -> this.onValueTuple.Call                 vs
        | ArrayItem (arr, idx)                             -> this.onArrayItem.Call                  (arr, idx)
        | NamedItem (ex, acc)                              -> this.onNamedItem.Call                  (ex, acc)
        | ValueArray vs                                    -> this.onValueArray.Call                 vs
        | NewArray (bt, idx)                               -> this.onNewArray.Call                   (bt, idx)
        | IntLiteral i                                     -> this.onIntLiteral.Call                 i
        | BigIntLiteral b                                  -> this.onBigIntLiteral.Call              b
        | DoubleLiteral d                                  -> this.onDoubleLiteral.Call              d
        | BoolLiteral b                                    -> this.onBoolLiteral.Call                b
        | ResultLiteral r                                  -> this.onResultLiteral.Call              r
        | PauliLiteral p                                   -> this.onPauliLiteral.Call               p
        | StringLiteral (s, exs)                           -> this.onStringLiteral.Call              (s, exs)
        | RangeLiteral (lhs, rhs)                          -> this.onRangeLiteral.Call               (lhs, rhs)
        | CopyAndUpdate (lhs, accEx, rhs)                  -> this.onCopyAndUpdateExpression.Call    (lhs, accEx, rhs)
        | CONDITIONAL (cond, ifTrue, ifFalse)              -> this.onConditionalExpression.Call      (cond, ifTrue, ifFalse)
        | EQ (lhs,rhs)                                     -> this.onEquality.Call                   ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | NEQ (lhs,rhs)                                    -> this.onInequality.Call                 ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | LT (lhs,rhs)                                     -> this.onLessThan.Call                   ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | LTE (lhs,rhs)                                    -> this.onLessThanOrEqual.Call            ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | GT (lhs,rhs)                                     -> this.onGreaterThan.Call                ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | GTE (lhs,rhs)                                    -> this.onGreaterThanOrEqual.Call         ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | AND (lhs,rhs)                                    -> this.onLogicalAnd.Call                 ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | OR (lhs,rhs)                                     -> this.onLogicalOr.Call                  ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | ADD (lhs,rhs)                                    -> this.onAddition.Call                   ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | SUB (lhs,rhs)                                    -> this.onSubtraction.Call                ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | MUL (lhs,rhs)                                    -> this.onMultiplication.Call             ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | DIV (lhs,rhs)                                    -> this.onDivision.Call                   ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | POW (lhs,rhs)                                    -> this.onExponentiate.Call               ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | MOD (lhs,rhs)                                    -> this.onModulo.Call                     ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | LSHIFT (lhs,rhs)                                 -> this.onLeftShift.Call                  ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | RSHIFT (lhs,rhs)                                 -> this.onRightShift.Call                 ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | BXOR (lhs,rhs)                                   -> this.onBitwiseExclusiveOr.Call         ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | BOR (lhs,rhs)                                    -> this.onBitwiseOr.Call                  ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | BAND (lhs,rhs)                                   -> this.onBitwiseAnd.Call                 ((lhs, rhs)          |> this.beforeBinaryOperatorExpression.Call)
        | NOT ex                                           -> this.onLogicalNot.Call                 (ex                  |> this.beforeUnaryOperatorExpression.Call)
        | NEG ex                                           -> this.onNegative.Call                   (ex                  |> this.beforeUnaryOperatorExpression.Call)
        | BNOT ex                                          -> this.onBitwiseNot.Call                 (ex                  |> this.beforeUnaryOperatorExpression.Call)

    (*Expression Type*)

    member val onRangeInformation = MyEvent<QsRangeInfo> this.onRangeInformationDefault
    member this.onRangeInformationDefault r = r

    member val onCharacteristicsExpression = MyEvent<ResolvedCharacteristics> this.onCharacteristicsExpressionDefault
    member this.onCharacteristicsExpressionDefault fs = fs

    member val onCallableInformation = MyEvent<CallableInformation> this.onCallableInformationDefault
    member this.onCallableInformationDefault opInfo =
        let characteristics = this.onCharacteristicsExpression.Call opInfo.Characteristics
        let inferred = opInfo.InferredInformation
        CallableInformation.New (characteristics, inferred)

    member val onUserDefinedType = MyEvent<UserDefinedType, ExpressionType> (this.onUserDefinedTypeDefault, ExpressionType.UserDefinedType)
    member this.onUserDefinedTypeDefault udt =
        let ns, name = udt.Namespace, udt.Name
        let range = this.onRangeInformation.Call udt.Range
        UserDefinedType.New (ns, name, range)

    member val onTypeParameter = MyEvent<QsTypeParameter, ExpressionType> (this.onTypeParameterDefault, ExpressionType.TypeParameter)
    member this.onTypeParameterDefault tp =
        let origin = tp.Origin
        let name = tp.TypeName
        let range = this.onRangeInformation.Call tp.Range
        QsTypeParameter.New (origin, name, range)

    member val onUnitType = MyEvent<unit, ExpressionType> (this.onUnitTypeDefault, (fun () -> ExpressionType.UnitType))
    member this.onUnitTypeDefault () = ()

    member val onOperationType = MyEvent<(ResolvedType * ResolvedType) * CallableInformation, ExpressionType> (this.onOperationTypeDefault, ExpressionType.Operation)
    member this.onOperationTypeDefault ((it, ot), info) = ((this.onResolvedType.Call it, this.onResolvedType.Call ot), this.onCallableInformation.Call info)

    member val onFunctionType = MyEvent<ResolvedType * ResolvedType, ExpressionType> (this.onFunctionTypeDefault, ExpressionType.Function)
    member this.onFunctionTypeDefault (it, ot) = (this.onResolvedType.Call it, this.onResolvedType.Call ot)

    member val onTupleType = MyEvent<ImmutableArray<ResolvedType>, ExpressionType> (this.onTupleTypeDefault, ExpressionType.TupleType)
    member this.onTupleTypeDefault ts = (ts |> Seq.map this.onResolvedType.Call).ToImmutableArray()

    member val onArrayType = MyEvent<ResolvedType, ExpressionType> (this.onArrayTypeDefault, ExpressionType.ArrayType)
    member this.onArrayTypeDefault b = this.onResolvedType.Call b

    member val onQubit = MyEvent<unit, ExpressionType> (this.onQubitDefault, (fun () -> ExpressionType.Qubit))
    member this.onQubitDefault () = ()

    member val onMissingType = MyEvent<unit, ExpressionType> (this.onMissingTypeDefault, (fun () -> ExpressionType.MissingType))
    member this.onMissingTypeDefault () = ()

    member val onInvalidType = MyEvent<unit, ExpressionType> (this.onInvalidTypeDefault, (fun () -> ExpressionType.InvalidType))
    member this.onInvalidTypeDefault () = ()

    member val onInt = MyEvent<unit, ExpressionType> (this.onIntDefault, (fun () -> ExpressionType.Int))
    member this.onIntDefault () = ()

    member val onBigInt = MyEvent<unit, ExpressionType> (this.onBigIntDefault, (fun () -> ExpressionType.BigInt))
    member this.onBigIntDefault () = ()

    member val onDouble = MyEvent<unit, ExpressionType> (this.onDoubleDefault, (fun () -> ExpressionType.Double))
    member this.onDoubleDefault () = ()

    member val onBool = MyEvent<unit, ExpressionType> (this.onBoolDefault, (fun () -> ExpressionType.Bool))
    member this.onBoolDefault () = ()

    member val onString = MyEvent<unit, ExpressionType> (this.onStringDefault, (fun () -> ExpressionType.String))
    member this.onStringDefault () = ()

    member val onResult = MyEvent<unit, ExpressionType> (this.onResultDefault, (fun () -> ExpressionType.Result))
    member this.onResultDefault () = ()

    member val onPauli = MyEvent<unit, ExpressionType> (this.onPauliDefault, (fun () -> ExpressionType.Pauli))
    member this.onPauliDefault () = ()

    member val onRange = MyEvent<unit, ExpressionType> (this.onRangeDefault, (fun () -> ExpressionType.Range))
    member this.onRangeDefault () = ()

    //member val onResolvedType = MyEvent<ResolvedType> this.onResolvedTypeDefault
    member this.onResolvedTypeDefault t =
        match t.Resolution with
        | ExpressionType.UnitType                    -> this.onUnitType.Call ()
        | ExpressionType.Operation ((it, ot), fs)    -> this.onOperationType.Call ((it, ot), fs)
        | ExpressionType.Function (it, ot)           -> this.onFunctionType.Call (it, ot)
        | ExpressionType.TupleType ts                -> this.onTupleType.Call ts
        | ExpressionType.ArrayType b                 -> this.onArrayType.Call b
        | ExpressionType.UserDefinedType udt         -> this.onUserDefinedType.Call udt
        | ExpressionType.TypeParameter tp            -> this.onTypeParameter.Call tp
        | ExpressionType.Qubit                       -> this.onQubit.Call ()
        | ExpressionType.MissingType                 -> this.onMissingType.Call ()
        | ExpressionType.InvalidType                 -> this.onInvalidType.Call ()
        | ExpressionType.Int                         -> this.onInt.Call ()
        | ExpressionType.BigInt                      -> this.onBigInt.Call ()
        | ExpressionType.Double                      -> this.onDouble.Call ()
        | ExpressionType.Bool                        -> this.onBool.Call ()
        | ExpressionType.String                      -> this.onString.Call ()
        | ExpressionType.Result                      -> this.onResult.Call ()
        | ExpressionType.Pauli                       -> this.onPauli.Call ()
        | ExpressionType.Range                       -> this.onRange.Call ()
        |> ResolvedType.New

    (*Expression*)

    member val onLocation = MyEvent<QsNullable<QsLocation>> this.onLocationDefault
    member this.onLocationDefault loc = loc

    member val onExpressionInformation = MyEvent<InferredExpressionInformation> this.onExpressionInformationDefault
    member this.onExpressionInformationDefault info = info

    member val onTypeParamResolutions = MyEvent<ImmutableDictionary<(QsQualifiedName*NonNullable<string>), ResolvedType>> this.onTypeParamResolutionsDefault
    member this.onTypeParamResolutionsDefault typeParams =
        let asTypeParameter (key) = QsTypeParameter.New (fst key, snd key, Null)
        let filteredTypeParams =
            typeParams
            |> Seq.map (fun kv -> this.onTypeParameter.Call (kv.Key |> asTypeParameter), kv.Value)
            |> Seq.choose (function | TypeParameter tp, value -> Some ((tp.Origin, tp.TypeName), this.onResolvedType.Call value) | _ -> None)
        filteredTypeParams.ToImmutableDictionary (fst,snd)

    //member val onTypedExpression = MyEvent<TypedExpression> this.onTypedExpressionDefault
    member this.onTypedExpressionDefault ex =
        let range                = this.onRangeInformation.Call ex.Range
        let typeParamResolutions = this.onTypeParamResolutions.Call ex.TypeParameterResolutions
        let kind                 = this.onExpressionKind.Call ex.Expression
        let exType               = this.onResolvedType.Call ex.ResolvedType
        let inferredInfo         = this.onExpressionInformation.Call ex.InferredInformation
        TypedExpression.New (kind, typeParamResolutions, exType, inferredInfo, range)

    (*Statement Kind*)

    member val onQubitInitializer = MyEvent<ResolvedInitializer> this.onQubitInitializerDefault
    member this.onQubitInitializerDefault init =
        match init.Resolution with
        | SingleQubitAllocation      -> SingleQubitAllocation
        | QubitRegisterAllocation ex -> QubitRegisterAllocation (this.onTypedExpression.Call ex)
        | QubitTupleAllocation is    -> QubitTupleAllocation ((is |> Seq.map this.onQubitInitializer.Call).ToImmutableArray())
        | InvalidInitializer         -> InvalidInitializer
        |> ResolvedInitializer.New

    member val beforeVariableDeclaration = MyEvent<SymbolTuple> this.beforeVariableDeclarationDefault
    member this.beforeVariableDeclarationDefault syms = syms

    member val onSymbolTuple = MyEvent<SymbolTuple> this.onSymbolTupleDefault
    member this.onSymbolTupleDefault syms = syms


    member val onExpressionStatement = MyEvent<TypedExpression, QsStatementKind> (this.onExpressionStatementDefault, QsExpressionStatement)
    member this.onExpressionStatementDefault ex = this.onTypedExpression.Call ex

    member val onReturnStatement = MyEvent<TypedExpression, QsStatementKind> (this.onReturnStatementDefault, QsReturnStatement)
    member this.onReturnStatementDefault ex = this.onTypedExpression.Call ex

    member val onFailStatement = MyEvent<TypedExpression, QsStatementKind> (this.onFailStatementDefault, QsFailStatement)
    member this.onFailStatementDefault ex = this.onTypedExpression.Call ex

    member val onVariableDeclaration = MyEvent<QsBinding<TypedExpression>, QsStatementKind> (this.onVariableDeclarationDefault, QsVariableDeclaration)
    member this.onVariableDeclarationDefault stm =
        let rhs = this.onTypedExpression.Call stm.Rhs
        let lhs = this.onSymbolTuple.Call stm.Lhs
        QsBinding<TypedExpression>.New stm.Kind (lhs, rhs)

    member val onValueUpdate = MyEvent<QsValueUpdate, QsStatementKind> (this.onValueUpdateDefault, QsValueUpdate)
    member this.onValueUpdateDefault stm =
        let rhs = this.onTypedExpression.Call stm.Rhs
        let lhs = this.onTypedExpression.Call stm.Lhs
        QsValueUpdate.New (lhs, rhs)

    member val onScope = MyEvent<QsScope> this.onScopeDefault

    member val onPositionedBlock = MyEvent<TypedExpression option * QsPositionedBlock> this.onPositionedBlockDefault
    member this.onPositionedBlockDefault (intro : TypedExpression option, block : QsPositionedBlock) =
        let location = this.onLocation.Call block.Location
        let comments = block.Comments
        let expr = intro |> Option.map this.onTypedExpression.Call
        let body = this.onScope.Call block.Body
        expr, QsPositionedBlock.New comments location body

    member val onConditionalStatement = MyEvent<QsConditionalStatement, QsStatementKind> (this.onConditionalStatementDefault, QsConditionalStatement)
    member this.onConditionalStatementDefault stm =
        let cases = stm.ConditionalBlocks |> Seq.map (fun (c, b) ->
            let cond, block = this.onPositionedBlock.Call(Some c, b)
            cond |> Option.get, block)
        let defaultCase = stm.Default |> QsNullable<_>.Map (fun b -> this.onPositionedBlock.Call(None, b) |> snd)
        QsConditionalStatement.New (cases, defaultCase)

    member val onForStatement = MyEvent<QsForStatement, QsStatementKind> (this.onForStatementDefault, QsForStatement)
    member this.onForStatementDefault stm =
        let iterVals = this.onTypedExpression.Call stm.IterationValues
        let loopVar = fst stm.LoopItem |> this.onSymbolTuple.Call
        let loopVarType = this.onResolvedType.Call (snd stm.LoopItem)
        let body = this.onScope.Call stm.Body
        QsForStatement.New ((loopVar, loopVarType), iterVals, body)

    member val onWhileStatement = MyEvent<QsWhileStatement, QsStatementKind> (this.onWhileStatementDefault, QsWhileStatement)
    member this.onWhileStatementDefault stm =
        let condition = this.onTypedExpression.Call stm.Condition
        let body = this.onScope.Call stm.Body
        QsWhileStatement.New (condition, body)

    member val onRepeatStatement = MyEvent<QsRepeatStatement, QsStatementKind> (this.onRepeatStatementDefault, QsRepeatStatement)
    member this.onRepeatStatementDefault stm =
        let repeatBlock = this.onPositionedBlock.Call(None, stm.RepeatBlock) |> snd
        let successCondition, fixupBlock = this.onPositionedBlock.Call(Some stm.SuccessCondition, stm.FixupBlock)
        QsRepeatStatement.New (repeatBlock, successCondition |> Option.get, fixupBlock)

    member val onConjugation = MyEvent<QsConjugation, QsStatementKind> (this.onConjugationDefault, QsConjugation)
    member this.onConjugationDefault stm =
        let outer = this.onPositionedBlock.Call (None, stm.OuterTransformation) |> snd
        let inner = this.onPositionedBlock.Call (None, stm.InnerTransformation) |> snd
        QsConjugation.New (outer, inner)

    member val onQubitScope : MyEvent<QsQubitScope, QsStatementKind> = MyEvent<QsQubitScope, QsStatementKind> (this.onQubitScopeDefault, QsQubitScope)
    member this.onQubitScopeDefault (stm : QsQubitScope) =
        this.onQubitScope.Call stm |> ignore
        let kind = stm.Kind
        let rhs = this.onQubitInitializer.Call stm.Binding.Rhs
        let lhs = this.onSymbolTuple.Call stm.Binding.Lhs
        let body = this.onScope.Call stm.Body
        QsQubitScope.New kind ((lhs, rhs), body)

    member val onAllocateQubits = MyEvent<QsQubitScope, QsStatementKind> (this.onAllocateQubitsDefault, (fun x -> this.onQubitScope.Call x))
    member this.onAllocateQubitsDefault stm = stm

    member val onBorrowQubits = MyEvent<QsQubitScope, QsStatementKind> (this.onBorrowQubitsDefault, (fun x -> this.onQubitScope.Call x))
    member this.onBorrowQubitsDefault stm = stm


    member private this.dispatchQubitScope (stm : QsQubitScope) =
        match stm.Kind with
        | Allocate -> this.onAllocateQubits.Call stm
        | Borrow   -> this.onBorrowQubits.Call stm

    member val onStatementKind = MyEvent<QsStatementKind> this.onStatementKindDefault
    member this.onStatementKindDefault kind =
        let beforeBinding (stm : QsBinding<TypedExpression>) = { stm with Lhs = this.beforeVariableDeclaration.Call stm.Lhs }
        let beforeForStatement (stm : QsForStatement) = {stm with LoopItem = (this.beforeVariableDeclaration.Call (fst stm.LoopItem), snd stm.LoopItem)}
        let beforeQubitScope (stm : QsQubitScope) = {stm with Binding = {stm.Binding with Lhs = this.beforeVariableDeclaration.Call stm.Binding.Lhs}}

        match kind with
        | QsExpressionStatement ex   -> this.onExpressionStatement.Call  (ex)
        | QsReturnStatement ex       -> this.onReturnStatement.Call      (ex)
        | QsFailStatement ex         -> this.onFailStatement.Call        (ex)
        | QsVariableDeclaration stm  -> this.onVariableDeclaration.Call  (stm  |> beforeBinding)
        | QsValueUpdate stm          -> this.onValueUpdate.Call          (stm)
        | QsConditionalStatement stm -> this.onConditionalStatement.Call (stm)
        | QsForStatement stm         -> this.onForStatement.Call         (stm  |> beforeForStatement)
        | QsWhileStatement stm       -> this.onWhileStatement.Call       (stm)
        | QsRepeatStatement stm      -> this.onRepeatStatement.Call      (stm)
        | QsConjugation stm          -> this.onConjugation.Call          (stm)
        | QsQubitScope stm           -> this.dispatchQubitScope          (stm  |> beforeQubitScope)

    (*Scope*)

    member val onLocalDeclarations = MyEvent<LocalDeclarations> this.onLocalDeclarationsDefault
    member this.onLocalDeclarationsDefault decl =
        let onLocalVariableDeclaration (local : LocalVariableDeclaration<NonNullable<string>>) =
            let loc = local.Position, local.Range
            let info = this.onExpressionInformation.Call local.InferredInformation
            let varType = this.onResolvedType.Call local.Type
            LocalVariableDeclaration.New info.IsMutable (loc, local.VariableName, varType, info.HasLocalQuantumDependency)
        let variableDeclarations = decl.Variables |> Seq.map onLocalVariableDeclaration |> ImmutableArray.CreateRange
        LocalDeclarations.New variableDeclarations

    member val onStatement = MyEvent<QsStatement> this.onStatementDefault
    member this.onStatementDefault stm =
        let location = this.onLocation.Call stm.Location
        let comments = stm.Comments
        let kind = this.onStatementKind.Call stm.Statement
        let varDecl = this.onLocalDeclarations.Call stm.SymbolDeclarations
        QsStatement.New comments location (kind, varDecl)

    //member val onScope = MyEvent<QsScope> this.onScopeDefault
    member this.onScopeDefault scope =
        let parentSymbols = this.onLocalDeclarations.Call scope.KnownSymbols
        let statements = scope.Statements |> Seq.map this.onStatement.Call
        QsScope.New (statements, parentSymbols)

    (*Syntax Tree*)

    member val beforeNamespaceElement = MyEvent<QsNamespaceElement> this.beforeNamespaceElementDefault
    member this.beforeNamespaceElementDefault e = e

    member val beforeCallable = MyEvent<QsCallable> this.beforeCallableDefault
    member this.beforeCallableDefault c = c

    member val beforeSpecialization = MyEvent<QsSpecialization> this.beforeSpecializationDefault
    member this.beforeSpecializationDefault spec = spec

    member val beforeSpecializationImplementation = MyEvent<SpecializationImplementation> this.beforeSpecializationImplementationDefault
    member this.beforeSpecializationImplementationDefault impl = impl

    member val beforeGeneratedImplementation = MyEvent<QsGeneratorDirective> this.beforeGeneratedImplementationDefault
    member this.beforeGeneratedImplementationDefault dir = dir

    member val onDocumentation = MyEvent<ImmutableArray<string>> this.onDocumentationDefault
    member this.onDocumentationDefault doc = doc

    member val onSourceFile = MyEvent<NonNullable<string>> this.onSourceFileDefault
    member this.onSourceFileDefault f = f

    member val onTypeItems = MyEvent<QsTuple<QsTypeItem>> this.onTypeItemsDefault
    member this.onTypeItemsDefault tItem =
        match tItem with
        | QsTuple items -> (items |> Seq.map this.onTypeItems.Call).ToImmutableArray() |> QsTuple
        | QsTupleItem (Anonymous itemType) ->
            let t = this.onResolvedType.Call itemType
            Anonymous t |> QsTupleItem
        | QsTupleItem (Named item) ->
            let loc  = item.Position, item.Range
            let t    = this.onResolvedType.Call item.Type
            let info = this.onExpressionInformation.Call item.InferredInformation
            LocalVariableDeclaration<_>.New info.IsMutable (loc, item.VariableName, t, info.HasLocalQuantumDependency) |> Named |> QsTupleItem

    member val onArgumentTuple = MyEvent<QsArgumentTuple> this.onArgumentTupleDefault
    member this.onArgumentTupleDefault arg =
        match arg with
        | QsTuple items -> (items |> Seq.map this.onArgumentTuple.Call).ToImmutableArray() |> QsTuple
        | QsTupleItem item ->
            let loc  = item.Position, item.Range
            let t    = this.onResolvedType.Call item.Type
            let info = this.onExpressionInformation.Call item.InferredInformation
            LocalVariableDeclaration<_>.New info.IsMutable (loc, item.VariableName, t, info.HasLocalQuantumDependency) |> QsTupleItem

    member val onSignature = MyEvent<ResolvedSignature> this.onSignatureDefault
    member this.onSignatureDefault s =
        let typeParams = s.TypeParameters
        let argType = this.onResolvedType.Call s.ArgumentType
        let returnType = this.onResolvedType.Call s.ReturnType
        let info = this.onCallableInformation.Call s.Information
        ResolvedSignature.New ((argType, returnType), info, typeParams)

    member val onExternalImplementation = MyEvent<unit> this.onExternalImplementationDefault
    member this.onExternalImplementationDefault () = ()

    member val onIntrinsicImplementation = MyEvent<unit> this.onIntrinsicImplementationDefault
    member this.onIntrinsicImplementationDefault () = ()

    member val onProvidedImplementation = MyEvent<QsArgumentTuple * QsScope> this.onProvidedImplementationDefault
    member this.onProvidedImplementationDefault (argTuple, body) =
        let argTuple = this.onArgumentTuple.Call argTuple
        let body = this.onScope.Call body
        argTuple, body

    member val onSelfInverseDirective = MyEvent<unit> this.onSelfInverseDirectiveDefault
    member this.onSelfInverseDirectiveDefault () = ()

    member val onInvertDirective = MyEvent<unit> this.onInvertDirectiveDefault
    member this.onInvertDirectiveDefault () = ()

    member val onDistributeDirective = MyEvent<unit> this.onDistributeDirectiveDefault
    member this.onDistributeDirectiveDefault () = ()

    member val onInvalidGeneratorDirective = MyEvent<unit> this.onInvalidGeneratorDirectiveDefault
    member this.onInvalidGeneratorDirectiveDefault () = ()

    member this.dispatchGeneratedImplementation dir =
        match this.beforeGeneratedImplementation.Call dir with
        | SelfInverse      -> this.onSelfInverseDirective.Call();     SelfInverse
        | Invert           -> this.onInvertDirective.Call();           Invert
        | Distribute       -> this.onDistributeDirective.Call();       Distribute
        | InvalidGenerator -> this.onInvalidGeneratorDirective.Call(); InvalidGenerator

    member this.dispatchSpecializationImplementation impl =
        match this.beforeSpecializationImplementation.Call impl with
        | External                  -> this.onExternalImplementation.Call();                  External
        | Intrinsic                 -> this.onIntrinsicImplementation.Call();                 Intrinsic
        | Generated dir             -> this.dispatchGeneratedImplementation dir       |> Generated
        | Provided (argTuple, body) -> this.onProvidedImplementation.Call (argTuple, body) |> Provided

    member val onAttribute = MyEvent<QsDeclarationAttribute> this.onAttributeDefault

    member val onSpecializationImplementation = MyEvent<QsSpecialization> this.onSpecializationImplementationDefault
    member this.onSpecializationImplementationDefault spec =
        let source = this.onSourceFile.Call spec.SourceFile
        let loc = this.onLocation.Call spec.Location
        let attributes = spec.Attributes |> Seq.map this.onAttribute.Call |> ImmutableArray.CreateRange
        let typeArgs = spec.TypeArguments |> QsNullable<_>.Map (fun args -> (args |> Seq.map this.onResolvedType.Call).ToImmutableArray())
        let signature = this.onSignature.Call spec.Signature
        let impl = this.dispatchSpecializationImplementation spec.Implementation
        let doc = this.onDocumentation.Call spec.Documentation
        let comments = spec.Comments
        QsSpecialization.New spec.Kind (source, loc) (spec.Parent, attributes, typeArgs, signature, impl, doc, comments)

    member val onBodySpecialization = MyEvent<QsSpecialization> this.onBodySpecializationDefault
    member this.onBodySpecializationDefault spec = this.onSpecializationImplementation.Call spec

    member val onAdjointSpecialization = MyEvent<QsSpecialization> this.onAdjointSpecializationDefault
    member this.onAdjointSpecializationDefault spec = this.onSpecializationImplementation.Call spec

    member val onControlledSpecialization = MyEvent<QsSpecialization> this.onControlledSpecializationDefault
    member this.onControlledSpecializationDefault spec = this.onSpecializationImplementation.Call spec

    member val onControlledAdjointSpecialization = MyEvent<QsSpecialization> this.onControlledAdjointSpecializationDefault
    member this.onControlledAdjointSpecializationDefault spec = this.onSpecializationImplementation.Call spec

    member this.dispatchSpecialization spec =
        let spec = this.beforeSpecialization.Call spec
        match spec.Kind with
        | QsSpecializationKind.QsBody               -> this.onBodySpecialization.Call spec
        | QsSpecializationKind.QsAdjoint            -> this.onAdjointSpecialization.Call spec
        | QsSpecializationKind.QsControlled         -> this.onControlledSpecialization.Call spec
        | QsSpecializationKind.QsControlledAdjoint  -> this.onControlledAdjointSpecialization.Call spec

    member val onType = MyEvent<QsCustomType> this.onTypeDefault
    member this.onTypeDefault t =
        let source = this.onSourceFile.Call t.SourceFile
        let loc = this.onLocation.Call t.Location
        let attributes = t.Attributes |> Seq.map this.onAttribute.Call |> ImmutableArray.CreateRange
        let underlyingType = this.onResolvedType.Call t.Type
        let typeItems = this.onTypeItems.Call t.TypeItems
        let doc = this.onDocumentation.Call t.Documentation
        let comments = t.Comments
        QsCustomType.New (source, loc) (t.FullName, attributes, typeItems, underlyingType, doc, comments)

    member val onCallableImplementation = MyEvent<QsCallable> this.onCallableImplementationDefault
    member this.onCallableImplementationDefault c =
        let source = this.onSourceFile.Call c.SourceFile
        let loc = this.onLocation.Call c.Location
        let attributes = c.Attributes |> Seq.map this.onAttribute.Call |> ImmutableArray.CreateRange
        let signature = this.onSignature.Call c.Signature
        let argTuple = this.onArgumentTuple.Call c.ArgumentTuple
        let specializations = c.Specializations |> Seq.map this.dispatchSpecialization
        let doc = this.onDocumentation.Call c.Documentation
        let comments = c.Comments
        QsCallable.New c.Kind (source, loc) (c.FullName, attributes, argTuple, signature, specializations, doc, comments)

    member val onOperation = MyEvent<QsCallable> this.onOperationDefault
    member this.onOperationDefault c = this.onCallableImplementation.Call c

    member val onFunction = MyEvent<QsCallable> this.onFunctionDefault
    member this.onFunctionDefault c = this.onCallableImplementation.Call c

    member val onTypeConstructor = MyEvent<QsCallable> this.onTypeConstructorDefault
    member this.onTypeConstructorDefault c = this.onCallableImplementation.Call c

    member this.dispatchCallable c =
        let c = this.beforeCallable.Call c
        match c.Kind with
        | QsCallableKind.Function           -> this.onFunction.Call c
        | QsCallableKind.Operation          -> this.onOperation.Call c
        | QsCallableKind.TypeConstructor    -> this.onTypeConstructor.Call c

    //member val onAttribute = MyEvent<QsDeclarationAttribute> this.onAttributeDefault
    member this.onAttributeDefault att = att

    member this.dispatchNamespaceElement element =
        match this.beforeNamespaceElement.Call element with
        | QsCustomType t    -> t |> this.onType.Call      |> QsCustomType
        | QsCallable c      -> c |> this.dispatchCallable |> QsCallable

    member val onNamespace = MyEvent<QsNamespace> this.onNamespaceDefault
    member this.onNamespaceDefault ns =
        let name = ns.Name
        let doc = ns.Documentation.AsEnumerable().SelectMany(fun entry ->
            entry |> Seq.map (fun doc -> entry.Key, this.onDocumentation.Call doc)).ToLookup(fst, snd)
        let elements = ns.Elements |> Seq.map this.dispatchNamespaceElement
        QsNamespace.New (name, elements, doc)