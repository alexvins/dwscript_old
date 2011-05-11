{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{$I dws.inc}
unit dwsCoreExprs;

interface

uses Windows, Classes, Variants, SysUtils, dwsSymbols, dwsErrors, dwsStrings,
   dwsStack, dwsExprs, dwsUtils, dwsTokenizer, dwsRelExprs;

type

   TCaseCondition = class;

   IVarParamData = interface
      function GetData: TData;
      function GetAddr: Integer;
      property Data: TData read GetData;
      property Addr: Integer read GetAddr;
   end;

   TVarExpr = class (TDataExpr)
      protected
         FStackAddr : Integer;

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor Create(prog: TdwsProgram; typ: TTypeSymbol; dataSym : TDataSymbol);
         class function CreateTyped(prog: TdwsProgram; typ: TTypeSymbol; dataSym : TDataSymbol) : TVarExpr;

         procedure AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer); override;
         procedure AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr); override;
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const Value: Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const Value: Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const Value: String); override;

         function Eval(exec : TdwsExecution) : Variant; override;

         function SameVarAs(expr : TVarExpr) : Boolean;

         property StackAddr : Integer read FStackAddr;
   end;

   TIntVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const Value: Int64); override;
         procedure AssignValueAsPInteger(exec : TdwsExecution; const pValue: PInt64);
         procedure IncValue(exec : TdwsExecution; const value: Int64);
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         function  EvalAsPInteger(exec : TdwsExecution) : PInt64; inline;
   end;

   TFloatVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const Value: Double); override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   TStrVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsString(exec : TdwsExecution; const Value: String); override;
         function  SetChar(exec : TdwsExecution; index : Integer; c : Char) : Boolean;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         procedure Append(exec : TdwsExecution; const value : String);
   end;

   TBoolVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TObjectVarExpr = class (TVarExpr)
      protected
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   TVarParentExpr = class(TVarExpr)
   protected
     FLevel: Integer;
     function GetAddr(exec : TdwsExecution) : Integer; override;
   public
     constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; DataSym: TDataSymbol);
   end;

   // Encapsulates a lazy parameter
   TLazyParamExpr = class(TTypedExpr)
      private
         FStackAddr : Integer;
         FLevel : Integer;
      public
         constructor Create(Prog: TdwsProgram; aTyp : TTypeSymbol; level, stackAddr : Integer);
         function Eval(exec : TdwsExecution) : Variant; override;
         property StackAddr : Integer read FStackAddr write FStackAddr;
         property Level : Integer read FLevel write FLevel;
   end;

   // Encapsulates a var parameter
   TVarParamExpr = class(TVarExpr)
   protected
     function GetAddr(exec : TdwsExecution) : Integer; override;
     function GetData(exec : TdwsExecution) : TData; override;
   public
     procedure AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer); override;
     procedure AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr); override;
     procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
     procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
     function  Eval(exec : TdwsExecution) : Variant; override;
   end;

   TConstParamExpr = class(TVarParamExpr)
      public
         function IsWritable : Boolean; override;
   end;

   // Encapsulates a var parameter
   TVarParamParentExpr = class(TVarParamExpr)
   protected
     FLevel: Integer;
     function GetAddr(exec : TdwsExecution) : Integer; override;
     function GetData(exec : TdwsExecution) : TData; override;
   public
     constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; DataSym: TDataSymbol);
   end;

   TConstParamParentExpr = class(TVarParamParentExpr)
   public
      function IsWritable : Boolean; override;
   end;

   // A constant value (like 0, 3.14159, 'Hello' or true)
   TConstExpr = class(TDataExpr)
   protected
     FData: TData;
     function GetData(exec : TdwsExecution) : TData; override;
   public
     constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); overload;
     constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData); overload;
     function Eval(exec : TdwsExecution) : Variant; override;
     function IsConstant : Boolean; override;
     function IsWritable : Boolean; override;

     class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant) : TConstExpr; overload; static;
     class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: String) : TConstExpr; overload; static;
     class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData) : TConstExpr; overload; static;
     class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr; overload; static;
   end;

   // TUnifiedConstList
   //
   TUnifiedConstList = class (TSortedList<TExprBase>)
      protected
         function Compare(const item1, item2 : TExprBase) : Integer; override;
      public
         destructor Destroy; override;
   end;

   TUnifiedConstExprClass = class of TUnifiedConstExpr;

   // TUnifiedConstExpr
   //
   {: Unified constants go into a program root unified const list. }
   TUnifiedConstExpr = class (TConstExpr)
      protected
         procedure DoNothing;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); virtual;
         class function CreateUnified(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant) : TUnifiedConstExpr;
         destructor DestroyTrue;
   end;

   // TConstBooleanExpr
   //
   TConstBooleanExpr = class(TUnifiedConstExpr)
      protected
         FValue : Boolean;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // TConstIntExpr
   //
   TConstIntExpr = class (TUnifiedConstExpr)
      private
         FValue : Int64;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Int64 read FValue;
   end;

   // TConstFloatExpr
   //
   TConstFloatExpr = class(TUnifiedConstExpr)
      private
         FValue : Double;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // TConstStringExpr
   //
   TConstStringExpr = class(TUnifiedConstExpr)
      private
         FValue : String;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         property Value : String read FValue write FValue;
   end;

   TArrayConstantExpr = class sealed (TPosDataExpr)
      protected
         FArrayAddr : Integer;
         FElementExprs : TTightList;

         function GetData(exec : TdwsExecution) : TData; override;
         function GetAddr(exec : TdwsExecution) : Integer; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);
         destructor Destroy; override;

         procedure AddElementExpr(Prog: TdwsProgram; ElementExpr: TTypedExpr);
         procedure Prepare(Prog: TdwsProgram; ElementTyp : TTypeSymbol);
         procedure TypeCheckElements(prog : TdwsProgram);
         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsTData(exec : TdwsExecution) : TData;
         function EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
         function IsConstant : Boolean; override;
         function IsWritable : Boolean; override;
   end;

   // Array expressions x[index]
   TArrayExpr = class(TPosDataExpr)
      protected
         FBaseExpr: TDataExpr;
         FIndexExpr: TTypedExpr;
         FElementSize: Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TTypedExpr);
         destructor Destroy; override;

         function IsWritable : Boolean; override;
   end;

   EScriptOutOfBounds = class (EScriptError);

   // Array expressions x[index] for static arrays
   TStaticArrayExpr = class(TArrayExpr)
   private
     FLowBound: Integer;
     FCount: Integer;
   protected
     function GetAddr(exec : TdwsExecution) : Integer; override;
     function GetData(exec : TdwsExecution) : TData; override;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos;
                        BaseExpr: TDataExpr; IndexExpr: TTypedExpr;
                        LowBound, HighBound: Integer);
   end;

   // Array expressions x[index] for open arrays
   TOpenArrayExpr = class(TArrayExpr)
   protected
     function GetAddr(exec : TdwsExecution) : Integer; override;
     function GetData(exec : TdwsExecution) : TData; override;
   end;

   // Array expressions: x[index0] for dynamic arrays
   TDynamicArrayExpr = class(TArrayExpr)
   protected
     function GetAddr(exec : TdwsExecution) : Integer; override;
     function GetData(exec : TdwsExecution) : TData; override;
   end;

   // Record expression: record.member
   TRecordExpr = class(TPosDataExpr)
      protected
         FBaseExpr : TDataExpr;
         FMemberOffset : Integer;

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; MemberSymbol: TMemberSymbol);
         destructor Destroy; override;

         function IsWritable : Boolean; override;
   end;

   TInitDataExpr = class(TNoResultExpr)
      protected
         FExpr : TDataExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TDataExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // Field expression: obj.Field
   TFieldExpr = class(TPosDataExpr)
      protected
         FObjectExpr: TDataExpr;
         FFieldAddr: Integer;

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Typ: TTypeSymbol;
                           FieldSym: TFieldSymbol; ObjExpr: TDataExpr);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   TReadOnlyFieldExpr = class(TFieldExpr)
      function IsWritable: Boolean; override;
   end;

   // length of dynamic arrays
   TArrayLengthExpr = class(TUnaryOpIntExpr)
      private
         FDelta : Integer;
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         property Delta : Integer read FDelta write FDelta;
   end;

   // length of an open array
   TOpenArrayLengthExpr = class(TArrayLengthExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // left[right] string read access
   TStringArrayOpExpr = class(TStringBinOpExpr)
      private
         FPos : TScriptPos;
      public
         constructor CreatePos(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TTypedExpr);
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         function ScriptPos : TScriptPos; override;
   end;

   TStringLengthExpr = class(TUnaryOpIntExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TAssignedExpr = class(TUnaryOpBoolExpr)
   end;

   TAssignedInstanceExpr = class(TAssignedExpr)
   public
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TAssignedMetaClassExpr = class(TAssignedExpr)
   public
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TOrdExpr = class(TUnaryOpIntExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TOrdIntExpr = class(TOrdExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TOrdBoolExpr = class(TOrdExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TOrdStrExpr = class(TOrdExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // obj is TMyClass
   TIsOpExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // obj as TMyClass
   TAsOpExpr = class(TBinaryOpExpr)
      private
         FPos : TScriptPos;

      public
         constructor CreateAs(Prog: TdwsProgram; Left, Right: TTypedExpr; const aPos : TScriptPos);
         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // obj left = obj right
   TObjCmpExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // -x
   TNegVariantExpr = class(TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TNegIntExpr = class (TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TNegFloatExpr = class (TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a + b
   TAddVariantExpr = class(TVariantBinOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TAddStrExpr = class(TStringBinOpExpr)
      procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
   end;
   TAddIntExpr = class(TIntegerBinOpExpr)
      function EvalAsInteger(exec : TdwsExecution) : Int64; override;
      function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;
   TAddFloatExpr = class(TFloatBinOpExpr)
      function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a - b
   TSubVariantExpr = class(TVariantBinOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TSubIntExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;
   TSubFloatExpr = class(TFloatBinOpExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a * b
   TMultVariantExpr = class(TVariantBinOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TMultIntExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TMultFloatExpr = class(TFloatBinOpExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // Sqr ( a )
   TSqrIntExpr = class(TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TSqrFloatExpr = class(TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a / b
   TDivideExpr = class(TFloatBinOpExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a div b
   TDivExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // a mod b
   TModExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // not bool a
   TNotBoolExpr = class(TUnaryOpBoolExpr)
      function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   // not int a
   TNotIntExpr = class(TUnaryOpIntExpr)
      function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   // not variant a
   TNotVariantExpr = class(TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // a and b
   TIntAndExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TBoolAndExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // a or b
   TIntOrExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TBoolOrExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // a xor b
   TIntXorExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TBoolXorExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // a implies b
   TBoolImpliesExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // a shl b
   TShlExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // a shr b
   TShrExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // newType(x)
   TConvExpr = class(TUnaryOpExpr)
      public
         class function WrapWithConvCast(prog : TdwsProgram; const scriptPos : TScriptPos;
                                         toTyp : TTypeSymbol; expr : TTypedExpr;
                                         reportError : Boolean) : TTypedExpr; static;
   end;

   // Float(x)
   TConvFloatExpr = class (TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // Integer(x)
   TConvIntegerExpr = class (TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // String(x)
   TConvStringExpr = class (TUnaryOpStringExpr)
     procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
   end;

   // Boolean(x)
   TConvBoolExpr = class (TUnaryOpBoolExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // Variant(simple)
   TConvVariantExpr = class (TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // Class(x)
   TConvClassExpr = class (TConvExpr)
     function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // Assert(condition, message);
   TAssertExpr = class(TNoResultExpr)
      protected
         FCond : TTypedExpr;
         FMessage : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; condExpr, msgExpr : TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // left := right;
   TAssignExpr = class(TNoResultExpr)
      protected
         FLeft : TDataExpr;
         FRight : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); virtual;
         destructor Destroy; override;

         property Left : TDataExpr read FLeft;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure TypeCheckAssign(prog : TdwsProgram); virtual;
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
         function  OptimizeConstAssignment(prog : TdwsProgram; exec : TdwsExecution) : TNoResultExpr;
   end;

   TAssignExprClass = class of TAssignExpr;

   // left := right; (class of)
   TAssignClassOfExpr = class(TAssignExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // left := right;
   TAssignDataExpr = class(TAssignExpr)
   protected
     FSize: Integer;
   public
     constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // left := right; (var, func)
   TAssignFuncExpr = class(TAssignExpr)
      public
         procedure TypeCheckAssign(prog : TdwsProgram); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // left := [constant array];
   TAssignArrayConstantExpr = class(TAssignDataExpr)
   public
     constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // var left := const right;
   TAssignConstDataToVarExpr = class(TAssignDataExpr)
   public
     constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // left := const right;
   TAssignConstExpr = class (TAssignExpr)
      public
         procedure TypeCheckAssign(prog : TdwsProgram); override;
   end;

   // left := const integer;
   TAssignConstToIntegerVarExpr = class(TAssignConstExpr)
      protected
         FRight : Int64;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Int64);
         procedure EvalNoResult(exec : TdwsExecution); override;
         property Right : Int64 read FRight write FRight;
   end;

   // left := const float;
   TAssignConstToFloatVarExpr = class(TAssignConstExpr)
      protected
         FRight : Double;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Double);
         procedure EvalNoResult(exec : TdwsExecution); override;
         property Right : Double read FRight write FRight;
   end;

   // left := const string;
   TAssignConstToStringVarExpr = class(TAssignConstExpr)
      protected
         FRight : String;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : String);
         procedure EvalNoResult(exec : TdwsExecution); override;
         property Right : String read FRight write FRight;
   end;

   // a := a op b
   TOpAssignExpr = class(TAssignExpr)
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // a += b
   TPlusAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a += b (int)
   TPlusAssignIntExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   // a += b (float)
   TPlusAssignFloatExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a += b (string)
   TPlusAssignStrExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // a -= b
   TMinusAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a -= b (int)
   TMinusAssignIntExpr = class(TMinusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   // a -= b (float)
   TMinusAssignFloatExpr = class(TMinusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a *= b
   TMultAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a *= b (int)
   TMultAssignIntExpr = class(TMultAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a *= b (float)
   TMultAssignFloatExpr = class(TMultAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a /= b
   TDivideAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a += b (int var)
   TIncIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a -= b (int var)
   TDecIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a += b (string var)
   TAppendStringVarExpr = class(TAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // (string var) += (string const)
   TAppendConstStringVarExpr = class(TAssignExpr)
      private
         FAppendString : String;
      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // val in [case conditions list]
   TInOpExpr = class(TTypedExpr)
      private
         FLeft : TTypedExpr;
         FCaseConditions : TTightList;

      public
         constructor Create(Prog: TdwsProgram; Left : TTypedExpr);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         function IsConstant : Boolean; override;
         procedure AddCaseCondition(cond : TCaseCondition);
   end;

   // statement; statement; statement;
   TBlockExpr = class(TBlockExprBase)
      private
         FTable : TSymbolTable;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;

         property  Table: TSymbolTable read FTable;
   end;

   // statement; statement; statement;
   TBlockExprNoTable = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBlockExprNoTable2 = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBlockExprNoTable3 = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBlockExprNoTable4 = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // if FCond then FThen
   TIfThenExpr = class(TNoResultExpr)
      private
         FCond : TTypedExpr;
         FThen : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const Pos : TScriptPos;
                            condExpr : TTypedExpr; thenExpr : TNoResultExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // if FCond then FThen else FElse
   TIfThenElseExpr = class(TIfThenExpr)
      private
         FElse : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const Pos : TScriptPos;
                            condExpr : TTypedExpr; thenExpr, elseExpr : TNoResultExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // Part of a case statement
   TCaseCondition = class
      private
         FOwnsTrueExpr : Boolean;
         FTrueExpr : TNoResultExpr;
         FPos : TScriptPos;

         function IsOfTypeNumber(prog : TdwsProgram; typ : TTypeSymbol) : Boolean;

      public
         constructor Create(const aPos : TScriptPos);
         destructor Destroy; override;

         function IsTrue(exec : TdwsExecution; const value: Variant) : Boolean; virtual; abstract;
         procedure TypeCheck(prog : TdwsProgram; typ : TTypeSymbol); virtual; abstract;
         function IsConstant : Boolean; virtual; abstract;

         property Pos : TScriptPos read FPos;
         property TrueExpr: TNoResultExpr read FTrueExpr write FTrueExpr;
         property OwnsTrueExpr: Boolean read FOwnsTrueExpr write FOwnsTrueExpr;
   end;

   TCaseConditions = TObjectList<TCaseCondition>;

   TCompareCaseCondition = class(TCaseCondition)
      private
         FCompareExpr : TTypedExpr;

      public
         constructor Create(const aPos : TScriptPos; compareExpr : TTypedExpr);
         destructor Destroy; override;

         function IsTrue(exec : TdwsExecution; const value : Variant) : Boolean; override;
         procedure TypeCheck(prog : TdwsProgram; typ : TTypeSymbol); override;
         function IsConstant : Boolean; override;
   end;

   TRangeCaseCondition = class(TCaseCondition)
      private
         FFromExpr : TTypedExpr;
         FToExpr : TTypedExpr;

      public
         constructor Create(const aPos : TScriptPos; fromExpr, toExpr : TTypedExpr);
         destructor Destroy; override;

         function IsTrue(exec : TdwsExecution; const Value: Variant): Boolean; override;
         procedure TypeCheck(prog : TdwsProgram; typ : TTypeSymbol); override;
         function IsConstant : Boolean; override;
   end;

   // case FValueExpr of {CaseConditions} else FElseExpr end;
   TCaseExpr = class(TNoResultExpr)
      private
         FCaseConditions: TTightList;
         FElseExpr: TNoResultExpr;
         FValueExpr: TTypedExpr;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure AddCaseCondition(cond : TCaseCondition);

         property ValueExpr: TTypedExpr read FValueExpr write FValueExpr;
         property ElseExpr: TNoResultExpr read FElseExpr write FElseExpr;
   end;

   // for FVarExpr := FFromExpr to FToExpr do FDoExpr;
   TForExpr = class(TNoResultExpr)
      private
         FDoExpr : TNoResultExpr;
         FFromExpr : TTypedExpr;
         FToExpr : TTypedExpr;
         FVarExpr : TIntVarExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         property DoExpr: TNoResultExpr read FDoExpr write FDoExpr;
         property FromExpr: TTypedExpr read FFromExpr write FFromExpr;
         property ToExpr: TTypedExpr read FToExpr write FToExpr;
         property VarExpr: TIntVarExpr read FVarExpr write FVarExpr;
   end;

   TForExprClass = class of TForExpr;

   TForUpwardExpr = class(TForExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TForDownwardExpr = class(TForExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // for FVarExpr := FFromExpr to FToExpr step FStepExpr do FDoExpr;
   TForStepExpr = class(TForExpr)
      private
         FStepExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         function EvalStep(exec : TdwsExecution) : Int64;

         property StepExpr : TTypedExpr read FStepExpr write FStepExpr;
   end;

   TFoSteprExprClass = class of TForStepExpr;

   TForUpwardStepExpr = class(TForStepExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TForDownwardStepExpr = class(TForStepExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // base class for while, repeat and infinite loops
   TLoopExpr = class(TNoResultExpr)
      private
         FCondExpr : TTypedExpr;
         FLoopExpr : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property CondExpr : TTypedExpr read FCondExpr write FCondExpr;
         property LoopExpr : TNoResultExpr read FLoopExpr write FLoopExpr;
   end;

   // while FCondExpr do FLoopExpr
   TWhileExpr = class(TLoopExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // repeat FLoopExpr while FCondExpr
   TRepeatExpr = class(TLoopExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   TFlowControlExpr = class(TNoResultExpr)
      public
   end;

   TBreakExpr = class(TFlowControlExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TExitExpr = class(TFlowControlExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TExitValueExpr = class(TExitExpr)
      private
         FAssignExpr : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; assignExpr : TNoResultExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TContinueExpr = class(TFlowControlExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TRaiseBaseExpr = class(TNoResultExpr)
   end;

   // raise TExceptionClass.Create;
   TRaiseExpr = class(TRaiseBaseExpr)
      private
         FExceptionExpr: TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; ExceptionExpr: TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TReraiseExpr = class(TRaiseBaseExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TExceptionExpr = class(TNoResultExpr)
      private
         FTryExpr : TNoResultExpr;
         FHandlerExpr : TNoResultExpr;

      protected
         function CreateEDelphiObj(exec : TdwsExecution; const ClassName, Message: string): IScriptObj;

         function EnterExceptionBlock(exec : TdwsExecution) : Variant;
         procedure LeaveExceptionBlock(exec : TdwsExecution);

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         property TryExpr : TNoResultExpr read FTryExpr write FTryExpr;
         property HandlerExpr : TNoResultExpr read FHandlerExpr write FHandlerExpr;
   end;

   TExceptDoExpr = class;

   // try FTryExpr except {FDoExprs}; else FElseExpr end;
   TExceptExpr = class(TExceptionExpr)
      private
         FDoExprs : TTightList;
         FElseExpr : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure AddDoExpr(expr : TExceptDoExpr);

         property ElseExpr: TNoResultExpr read FElseExpr write FElseExpr;
   end;

   // try..except on FExceptionVar: FExceptionVar.Typ do FDoBlockExpr; ... end;
   TExceptDoExpr = class(TNoResultExpr)
      private
         FExceptionVar : TDataSymbol;
         FDoBlockExpr : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property DoBlockExpr: TNoResultExpr read FDoBlockExpr write FDoBlockExpr;
         property ExceptionVar: TDataSymbol read FExceptionVar write FExceptionVar;
   end;

   // try FTryExpr finally FHandlerExpr end;
   TFinallyExpr = class(TExceptionExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TStringArraySetExpr = class(TNoResultExpr)
      private
         FStringExpr: TDataExpr;
         FIndexExpr: TTypedExpr;
         FValueExpr: TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; StringExpr : TDataExpr; IndexExpr, ValueExpr: TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TVarStringArraySetExpr = class(TStringArraySetExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TSpecialUnaryBoolExpr = class(TUnaryOpBoolExpr)
      public
         function IsConstant : Boolean; override;
   end;

   TDefinedExpr = class(TSpecialUnaryBoolExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TDeclaredExpr = class(TSpecialUnaryBoolExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         class function FindSymbol(symbolTable : TSymbolTable; const name : String) : TSymbol; static;
   end;

   TRegisteredBinaryOperator = record
      ExprClass : TProgramExprClass;
      LeftType : TTypeSymbol;
      RighType : TTypeSymbol;
   end;

   // lists of binary operators and their expression classes
   // used for operator overloading
   TBinaryOperators = class
      private
         FItems : array [TTokenType] of array of TRegisteredBinaryOperator;

      public
         constructor Create(table : TSymbolTable);

         procedure RegisterOperator(aToken : TTokenType; aExprClass : TProgramExprClass;
                                    aLeftType, aRightType : TTypeSymbol);

         function ExprClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TProgramExprClass;
         function BinaryOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TBinaryOpExprClass;
         function AssignmentOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TAssignExprClass;
         function RelOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TRelOpExprClass;
   end;

   EClassCast = class (EScriptError) end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsStringFunctions;

// ------------------
// ------------------ TVarExpr ------------------
// ------------------

// Create
//
constructor TVarExpr.Create(prog : TdwsProgram; typ : TTypeSymbol; dataSym : TDataSymbol);
begin
   inherited Create(Prog, Typ);
   FStackAddr:=DataSym.StackAddr;
end;

// CreateTyped
//
class function TVarExpr.CreateTyped(prog : TdwsProgram; typ : TTypeSymbol; dataSym : TDataSymbol) : TVarExpr;
begin
   if typ.IsOfType(prog.TypInteger) then
      Result:=TIntVarExpr.Create(prog, typ, dataSym)
   else if typ.IsOfType(prog.TypFloat) then
      Result:=TFloatVarExpr.Create(prog, typ, dataSym)
   else if typ.IsOfType(prog.TypString) then
      Result:=TStrVarExpr.Create(prog, typ, dataSym)
   else if typ.IsOfType(prog.TypBoolean) then
      Result:=TBoolVarExpr.Create(prog, typ, dataSym)
   else if typ is TClassSymbol then
      Result:=TObjectVarExpr.Create(prog, typ, dataSym)
   else Result:=TVarExpr.Create(prog, typ, dataSym);
end;

// Eval
//
function TVarExpr.Eval(exec : TdwsExecution) : Variant;
begin
   exec.Stack.ReadValue(Addr[exec], Result);
end;

// SameVarAs
//
function TVarExpr.SameVarAs(expr : TVarExpr) : Boolean;
begin
   Result:=    (FStackAddr=expr.FStackAddr)
           and (ClassType=expr.ClassType);
end;

// GetAddr
//
function TVarExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=exec.Stack.BasePointer+FStackAddr;
end;

// GetData
//
function TVarExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result:=exec.Stack.Data;
end;

// AssignData
//
procedure TVarExpr.AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer);
begin
   exec.Stack.WriteData(SourceAddr, Addr[exec], Typ.Size, SourceData);
end;

// AssignDataExpr
//
procedure TVarExpr.AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr);
begin
   exec.Stack.WriteData(DataExpr.Addr[exec], Addr[exec], Typ.Size, DataExpr.Data[exec]);
end;

// AssignExpr
//
procedure TVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
var
   buf : Variant;
begin
   Expr.EvalAsVariant(exec, buf);
   exec.Stack.WriteValue(Addr[exec], buf);
end;

// AssignValue
//
procedure TVarExpr.AssignValue(exec : TdwsExecution; const Value: Variant);
begin
   exec.Stack.WriteValue(Addr[exec], Value);
end;

// AssignValueAsInteger
//
procedure TVarExpr.AssignValueAsInteger(exec : TdwsExecution; const Value: Int64);
begin
   exec.Stack.WriteIntValue(Addr[exec], Value);
end;

// AssignValueAsBoolean
//
procedure TVarExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   exec.Stack.WriteBoolValue(Addr[exec], Value);
end;

// AssignValueAsFloat
//
procedure TVarExpr.AssignValueAsFloat(exec : TdwsExecution; const Value: Double);
begin
   exec.Stack.WriteFloatValue(Addr[exec], Value);
end;

// AssignValueAsString
//
procedure TVarExpr.AssignValueAsString(exec : TdwsExecution; const Value: String);
begin
   exec.Stack.WriteStrValue(Addr[exec], Value);
end;

// ------------------
// ------------------ TIntVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TIntVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   exec.Stack.WriteIntValue_BaseRelative(FStackAddr, Expr.EvalAsInteger(exec));
end;

// AssignValue
//
procedure TIntVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsInteger(exec, value);
end;

// AssignValueAsInteger
//
procedure TIntVarExpr.AssignValueAsInteger(exec : TdwsExecution; const value: Int64);
begin
   exec.Stack.WriteIntValue_BaseRelative(FStackAddr, value);
end;

// AssignValueAsPInteger
//
procedure TIntVarExpr.AssignValueAsPInteger(exec : TdwsExecution; const pValue: PInt64);
begin
   exec.Stack.WriteIntValue_BaseRelative(FStackAddr, pValue);
end;

// IncValue
//
procedure TIntVarExpr.IncValue(exec : TdwsExecution; const value: Int64);
begin
   exec.Stack.IncIntValue_BaseRelative(FStackAddr, value);
end;

// EvalAsInteger
//
function TIntVarExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=exec.Stack.ReadIntValue_BaseRelative(FStackAddr);
end;

// EvalAsFloat
//
function TIntVarExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=exec.Stack.ReadIntAsFloatValue_BaseRelative(FStackAddr);
end;

// EvalAsPInteger
//
function TIntVarExpr.EvalAsPInteger(exec : TdwsExecution) : PInt64;
begin
   Result:=exec.Stack.PointerToIntValue(exec.Stack.BasePointer+FStackAddr);
end;

// ------------------
// ------------------ TFloatVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TFloatVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   exec.Stack.WriteFloatValue_BaseRelative(FStackAddr, Expr.EvalAsFloat(exec));
end;

// AssignValue
//
procedure TFloatVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsFloat(exec, value);
end;

// AssignValueAsFloat
//
procedure TFloatVarExpr.AssignValueAsFloat(exec : TdwsExecution; const value: Double);
begin
   exec.Stack.WriteFloatValue_BaseRelative(FStackAddr, value);
end;

// EvalAsFloat
//
type
   TdwsExecutionCracker = class(TdwsExecution);
function TFloatVarExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$ifdef PUREPASCAL}
begin
   Result:=exec.Stack.PointerToFloatValue_BaseRelative(FStackAddr)^;
{$else}
asm
   lea   ecx, [edx].TdwsExecutionCracker.FStack
   mov   edx, [eax].FStackAddr
   mov   eax, ecx
   call  TStackMixIn.PointerToFloatValue_BaseRelative;
   fld   qword [eax]
{$endif}
end;

// ------------------
// ------------------ TStrVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TStrVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
var
   buf : String;
begin
   Expr.EvalAsString(exec, buf);
   exec.Stack.WriteStrValue(exec.Stack.BasePointer + FStackAddr, buf);
end;

// AssignValue
//
procedure TStrVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsString(exec, value);
end;

// AssignValueAsString
//
procedure TStrVarExpr.AssignValueAsString(exec : TdwsExecution; const value: String);
begin
   exec.Stack.WriteStrValue(exec.Stack.BasePointer + FStackAddr, value);
end;

// SetChar
//
function TStrVarExpr.SetChar(exec : TdwsExecution; index : Integer; c : Char) : Boolean;
begin
   Result:=exec.Stack.SetStrChar(exec.Stack.BasePointer + FStackAddr, index, c);
end;

// EvalAsString
//
procedure TStrVarExpr.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   exec.Stack.ReadStrValue(exec.Stack.BasePointer + FStackAddr, Result);
end;

// Append
//
procedure TStrVarExpr.Append(exec : TdwsExecution; const value : String);
begin
   exec.Stack.AppendStringValue_BaseRelative(FStackAddr, value);
end;

// ------------------
// ------------------ TBoolVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TBoolVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   exec.Stack.WriteBoolValue(exec.Stack.BasePointer + FStackAddr, Expr.EvalAsBoolean(exec));
end;

// AssignValue
//
procedure TBoolVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsBoolean(exec, value);
end;

// AssignValueAsBoolean
//
procedure TBoolVarExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   exec.Stack.WriteBoolValue(exec.Stack.BasePointer + FStackAddr, value);
end;

function TBoolVarExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=exec.Stack.ReadBoolValue(exec.Stack.BasePointer + FStackAddr);
end;

// EvalAsInteger
//
function TBoolVarExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Int64(exec.Stack.ReadBoolValue(exec.Stack.BasePointer + FStackAddr));
end;

// ------------------
// ------------------ TObjectVarExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TObjectVarExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
type
   PUnknown = ^IUnknown;
begin
   exec.Stack.ReadInterfaceValue(exec.Stack.BasePointer + FStackAddr, PUnknown(@Result)^);
end;

// ------------------
// ------------------ TVarParentExpr ------------------
// ------------------

// Create
//
constructor TVarParentExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; DataSym: TDataSymbol);
begin
   inherited;
   FLevel:=DataSym.Level;
end;

// GetAddr
//
function TVarParentExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=exec.Stack.GetSavedBp(FLevel)+FStackAddr;
end;

// ------------------
// ------------------ TVarParamExpr ------------------
// ------------------

// GetAddr
//
function TVarParamExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result := IVarParamData(IUnknown(exec.Stack.Data[exec.Stack.BasePointer + FStackAddr])).Addr;
end;

// GetData
//
function TVarParamExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result := IVarParamData(IUnknown(exec.Stack.Data[exec.Stack.BasePointer + FStackAddr])).Data;
end;

// AssignData
//
procedure TVarParamExpr.AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer);
begin
   DWSCopyData(SourceData, SourceAddr, Data[exec], Addr[exec], Typ.Size);
end;

// AssignValue
//
procedure TVarParamExpr.AssignValue(exec : TdwsExecution; const Value: Variant);
begin
   VarCopy(Data[exec][Addr[exec]], Value);
end;

// AssignExpr
//
procedure TVarParamExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   Expr.EvalAsVariant(exec, Data[exec][Addr[exec]]);
end;

// AssignDataExpr
//
procedure TVarParamExpr.AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr);
begin
   DWSCopyData(DataExpr.Data[exec], DataExpr.Addr[exec], Data[exec], Addr[exec], Typ.Size);
end;

// Eval
//
function TVarParamExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result := Data[exec][Addr[exec]];
end;

// ------------------
// ------------------ TConstParamExpr ------------------
// ------------------

// IsWritable
//
function TConstParamExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TVarParamParentExpr ------------------
// ------------------

// Create
//
constructor TVarParamParentExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; DataSym: TDataSymbol);
begin
   inherited;
   FLevel := DataSym.Level;
end;

// GetAddr
//
function TVarParamParentExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result := IVarParamData(IUnknown(exec.Stack.Data[exec.Stack.GetSavedBp(FLevel) + FStackAddr])).Addr;
end;

// GetData
//
function TVarParamParentExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result := IVarParamData(IUnknown(exec.Stack.Data[exec.Stack.GetSavedBp(FLevel) + FStackAddr])).Data;
end;

// ------------------
// ------------------ TConstParamParentExpr ------------------
// ------------------

// IsWritable
//
function TConstParamParentExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TUnifiedConstList<TExprBase> ------------------
// ------------------

// Compare
//
function TUnifiedConstList.Compare(const item1, item2 : TExprBase) : Integer;
var
   unified1, unified2 : TUnifiedConstExpr;
   vd1, vd2 : PVarData;
   rawResult : Int64;
begin
   unified1:=TUnifiedConstExpr(item1);
   unified2:=TUnifiedConstExpr(item2);
   if unified1.ClassType=unified2.ClassType then begin
      if unified1.Typ=unified2.Typ then begin
         vd1:=@unified1.FData[0];
         vd2:=@unified2.FData[0];
         rawResult:=Integer(vd1.VType)-Integer(vd2.VType);
         if rawResult=0 then begin
            case vd1.VType of
               varUString : rawResult:=CompareStr(String(vd1.VUString), String(vd2.VUString));
               varInt64 : rawResult:=vd1.VInt64-vd2.VInt64;
               varBoolean : rawResult:=Integer(vd1.VBoolean)-Integer(vd2.VBoolean);
            else
               case VarCompareValue(unified1.FData[0], unified2.FData[0]) of
                  vrEqual : rawResult:=0;
                  vrLessThan : rawResult:=-1;
                  vrGreaterThan : rawResult:=1;
               else
                  rawResult:=0;
                  Assert(False);
               end;
            end;
         end;
      end else rawResult:=NativeInt(unified1.Typ)-NativeInt(unified2.Typ);
   end else rawResult:=NativeInt(unified1.ClassType)-NativeInt(unified2.ClassType);

   if rawResult=0 then
      Result:=0
   else if rawResult>0 then
      Result:=1
   else Result:=-1;
end;

// Destroy
//
destructor TUnifiedConstList.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      TUnifiedConstExpr(Items[i]).DestroyTrue;
   inherited;
end;

// ------------------
// ------------------ TConstExpr ------------------
// ------------------

// Create
//
constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   inherited Create(Prog, Typ);
   Assert(Typ.Size=1);
   SetLength(FData, 1);
   FData[0] := Value;
end;

// Create
//
constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData);
begin
   inherited Create(Prog, Typ);
   FData := Data;
end;

// Eval
//
function TConstExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result := FData[0];
end;

// IsConstant
//
function TConstExpr.IsConstant : Boolean;
begin
   Result:=True;
end;

// IsWritable
//
function TConstExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant) : TConstExpr;
begin
   if Typ=Prog.TypString then
      Result:=TConstStringExpr.CreateUnified(Prog, Typ, Value)
   else if (Typ=Prog.TypInteger) or (Typ.Typ=Prog.TypInteger) then
      Result:=TConstIntExpr.CreateUnified(Prog, Typ, Value)
   else if Typ=Prog.TypBoolean then
      Result:=TConstBooleanExpr.CreateUnified(Prog, Typ, Value)
   else if Typ=Prog.TypFloat then
      Result:=TConstFloatExpr.CreateUnified(Prog, Typ, Value)
   else if Typ is TClassOfSymbol then begin
      Assert(VarType(Value) in [varInt64, varEmpty]);
      Result:=TConstExpr.Create(Prog, Typ, Value);
   end else Result:=TConstExpr.Create(Prog, Typ, Value);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: String) : TConstExpr;
begin
   Result:=CreateTyped(Prog, Typ, Variant(Value));
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData) : TConstExpr;
begin
   if Length(Data)=1 then
      Result:=TConstExpr.CreateTyped(Prog, Typ, Data[0])
   else Result:=TConstExpr.Create(Prog, Typ, Data);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr;
begin
   if constSymbol<>nil then
      Result:=CreateTyped(Prog, Typ, constSymbol.Data)
   else Result:=CreateTyped(Prog, Typ, Unassigned);
end;

// GetData
//
function TConstExpr.GetData(exec : TdwsExecution) : TData;
begin
  Result := FData;
end;

// ------------------
// ------------------ TUnifiedConstExpr ------------------
// ------------------

// Create
//
constructor TUnifiedConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   inherited Create(Prog, Typ, Value);
end;

// CreateUnified
//
class function TUnifiedConstExpr.CreateUnified(Prog: TdwsProgram; Typ: TTypeSymbol;
                                               const Value: Variant) : TUnifiedConstExpr;
const
   vmtDestroy = -4;
var
   i : Integer;
   p : Pointer;
   n : Cardinal;
   added : Boolean;
begin
   Result:=Self.Create(Prog, Typ, Value);

   i:=Prog.Root.UnifiedConstList.AddOrFind(Result, added);
   if not added then begin
      Result.DestroyTrue;
      Result:=TUnifiedConstExpr(Prog.Root.UnifiedConstList[i]);
      Exit;
   end;

   p:=@TUnifiedConstExpr.DoNothing;
   if PPointer(NativeInt(Self)+vmtDestroy)^<>p then begin
      WriteProcessMemory(GetCurrentProcess,
                         Pointer(NativeInt(Self)+vmtDestroy),
                         @p, SizeOf(Pointer), n);
   end;
end;

// DoNothing
//
procedure TUnifiedConstExpr.DoNothing;
begin
   // nothing
end;

// DestroyTrue
//
destructor TUnifiedConstExpr.DestroyTrue;
begin
   inherited Destroy;
end;

// ------------------
// ------------------ TConstBooleanExpr ------------------
// ------------------

// Create
//
constructor TConstBooleanExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypBoolean;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstBooleanExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Integer(FValue);
end;

// EvalAsBoolean
//
function TConstBooleanExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TConstIntExpr ------------------
// ------------------

// Create
//
constructor TConstIntExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypInteger;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
{$ifdef PUREPASCAL}
begin
   Result:=FValue;
{$else}
asm
   mov   edx, [eax + OFFSET FValue + 4]
   mov   eax, [eax + OFFSET FValue]
{$endif}
end;

// EvalAsFloat
//
function TConstIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$ifdef PUREPASCAL}
begin
   Result:=FValue;
{$else}
asm
   fild  qword [eax + OFFSET FValue]
{$endif}
end;

// ------------------
// ------------------ TConstFloatExpr ------------------
// ------------------

// Create
//
constructor TConstFloatExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypFloat;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsFloat
//
function TConstFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$ifdef PUREPASCAL}
begin
   Result:=FValue;
{$else}
asm
   fld qword [eax].FValue
{$endif}
end;

// ------------------
// ------------------ TConstStringExpr ------------------
// ------------------

// Create
//
constructor TConstStringExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypString;
   UnifyAssignString(Value, FValue);
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsString
//
procedure TConstStringExpr.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TArrayExpr ------------------
// ------------------

// Create
//
constructor TArrayExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TTypedExpr);
begin
   inherited Create(Prog, Pos, BaseExpr.BaseType.Typ);
   FBaseExpr := BaseExpr;
   FIndexExpr := IndexExpr;
   FElementSize := Typ.Size; // Necessary because of arrays of records!
   FTyp:=FBaseExpr.Typ.Typ;
end;

// Destroy
//
destructor TArrayExpr.Destroy;
begin
   FBaseExpr.Free;
   FIndexExpr.Free;
   inherited;
end;

// IsWritable
//
function TArrayExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
end;

// GetSubExpr
//
function TArrayExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FBaseExpr;
      1 : Result:=FIndexExpr;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TArrayExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TStaticArrayExpr ------------------
// ------------------

// Create
//
constructor TStaticArrayExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
      BaseExpr: TDataExpr; IndexExpr: TTypedExpr; LowBound, HighBound: Integer);
begin
   inherited Create(Prog, Pos, BaseExpr, IndexExpr);
   FLowBound:=LowBound;
   FCount:=HighBound-LowBound+1;
end;

// GetAddr
//
function TStaticArrayExpr.GetAddr(exec : TdwsExecution) : Integer;
var
   index: Integer;
begin
   // Get index
   index := FIndexExpr.EvalAsInteger(exec) - FLowBound;

   if Cardinal(index)>=Cardinal(FCount) then begin
      if index>=FCount then
         RaiseUpperExceeded(exec, index)
      else RaiseLowerExceeded(exec, index);
   end;
   // Calculate the address
   Result := FBaseExpr.Addr[exec] + (index * FElementSize);
end;

// GetData
//
function TStaticArrayExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result := FBaseExpr.Data[exec];
end;

// ------------------
// ------------------ TOpenArrayExpr ------------------
// ------------------

// GetAddr
//
function TOpenArrayExpr.GetAddr(exec : TdwsExecution) : Integer;
var
   index, len: Integer;
begin
   index := FIndexExpr.EvalAsInteger(exec);

   len := Length(FBaseExpr.Data[exec]);

   if Cardinal(index)>=Cardinal(len) then begin
      if index >= len then
         RaiseUpperExceeded(exec, index)
      else if index < 0 then
         RaiseLowerExceeded(exec, index);
   end;
   // Calculate the address
   Result := index;
end;

// GetData
//
function TOpenArrayExpr.GetData(exec : TdwsExecution) : TData;
begin
  Result := FBaseExpr.Data[exec];
end;

// ------------------
// ------------------ TDynamicArrayExpr ------------------
// ------------------

// GetAddr
//
function TDynamicArrayExpr.GetAddr(exec : TdwsExecution) : Integer;
var
   index, length: Integer;
   baseAddr: Integer;
begin
   baseAddr := FBaseExpr.EvalAsInteger(exec);
   index := FIndexExpr.EvalAsInteger(exec);

   length := exec.Stack.Data[baseAddr - 1];

   if Cardinal(index)>=Cardinal(length) then begin
      if index >= length then
         RaiseUpperExceeded(exec, index)
      else if index < 0 then
         RaiseLowerExceeded(exec, index);
   end;
   // Calculate the address
   Result := baseAddr + (index * FElementSize);
end;

// GetData
//
function TDynamicArrayExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result := exec.Stack.Data;
end;

// ------------------
// ------------------ TArrayConstantExpr ------------------
// ------------------

// Create
//
constructor TArrayConstantExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos);
begin
   inherited Create(Prog, Pos, TStaticArraySymbol.Create('', Prog.TypNil, 0, -1));
end;

// Destroy
//
destructor TArrayConstantExpr.Destroy;
begin
   FElementExprs.Clean;
   FTyp.Free;
   inherited;
end;

// AddElementExpr
//
procedure TArrayConstantExpr.AddElementExpr(Prog: TdwsProgram; ElementExpr: TTypedExpr);
var
   arraySymbol : TStaticArraySymbol;
begin
   arraySymbol:=(FTyp as TStaticArraySymbol);
   if arraySymbol.Typ<>Prog.TypVariant then begin
      if arraySymbol.Typ=Prog.TypNil then
         arraySymbol.Typ:=ElementExpr.Typ
      else
      if arraySymbol.Typ<>ElementExpr.Typ then begin
         if arraySymbol.Typ=Prog.TypNil then
            arraySymbol.Typ:=ElementExpr.Typ
         else if (arraySymbol.Typ=Prog.TypInteger) and (ElementExpr.Typ=Prog.TypFloat) then
            arraySymbol.Typ:=Prog.TypFloat
         else if not ((arraySymbol.Typ=Prog.TypFloat) and (ElementExpr.Typ=Prog.TypInteger)) then
            arraySymbol.Typ:=Prog.TypVariant;
      end;
   end;
   FElementExprs.Add(ElementExpr);
   arraySymbol.AddElement;
end;

// Prepare
//
procedure TArrayConstantExpr.Prepare(Prog: TdwsProgram; ElementTyp : TTypeSymbol);
var
   x : Integer;
   elemExpr : TTypedExpr;
begin
   if (ElementTyp<>nil) and (FTyp.Typ <> ElementTyp) then begin
      // need a compatibility check here maybe???
      (FTyp as TStaticArraySymbol).Typ:=ElementTyp;
   end;

   for x := 0 to FElementExprs.Count - 1 do begin
      elemExpr:=FElementExprs.List[x];
      if elemExpr is TArrayConstantExpr then
         TArrayConstantExpr(elemExpr).Prepare(Prog, FTyp.Typ.Typ);
   end;

   FArrayAddr := Prog.GetGlobalAddr(FElementExprs.Count * FTyp.Typ.Size + 1);
end;

// GetData
//
function TArrayConstantExpr.GetData(exec : TdwsExecution) : TData;
begin
  Eval(exec);
  Result := exec.Stack.Data;// FData;
end;

// GetAddr
//
function TArrayConstantExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=FArrayAddr+1;
end;

// GetSubExpr
//
function TArrayConstantExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=TExprBase(FElementExprs.List[i]);
end;

// GetSubExprCount
//
function TArrayConstantExpr.GetSubExprCount : Integer;
begin
   Result:=FElementExprs.Count;
end;

function TArrayConstantExpr.Eval(exec : TdwsExecution) : Variant;
//var
//  x: Integer;
//  elemSize: Integer;
begin
   // at the moment, Eval shouldn't ever be invoked
//
//   if FElementExprs.Count>0 then
//      exec.Stack.WriteValue(FArrayAddr, FElementExprs.Count);
//
//  elemSize := Typ.Typ.Size;
//  if elemSize = 1 then
//  begin
//    for x := 0 to FElementExprs.Count - 1 do
//    begin
//      exec.Stack.WriteValue(FArrayAddr + 1 + x, TTypedExpr(FElementExprs.List[x]).Eval(exec));
//    end;
//  end
//  else begin
//    for x := 0 to FElementExprs.Count - 1 do
//    begin
//      exec.Stack.WriteData(
//        TDataExpr(FElementExprs.List[x]).Addr[exec],
//        FArrayAddr + 1 + x * elemSize,
//        elemSize,
//        TDataExpr(FElementExprs.List[x]).Data[exec]);
//    end;
//  end;
//  Result := FArrayAddr + 1;
end;

// EvalAsTData
//
function TArrayConstantExpr.EvalAsTData(exec : TdwsExecution) : TData;
var
   i : Integer;
   expr : TTypedExpr;
begin
   SetLength(Result, FElementExprs.Count);
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      expr.EvalAsVariant(exec, Result[i]);
   end;
end;

// EvalAsVarRecArray
//
function TArrayConstantExpr.EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;
var
   i : Integer;
   expr : TTypedExpr;
   buf : Variant;
begin
   Result:=TVarRecArrayContainer.Create;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      expr.EvalAsVariant(exec, buf);
      Result.Add(buf);
   end;
   Result.Initialize;
end;

// Optimize
//
function TArrayConstantExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   i : Integer;
   expr : TTypedExpr;
begin
   Result:=Self;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      FElementExprs.List[i]:=expr.Optimize(prog, exec);
   end;
end;

// IsConstant
//
function TArrayConstantExpr.IsConstant : Boolean;
var
   i : Integer;
begin
   for i:=0 to FElementExprs.Count-1 do
      if not TTypedExpr(FElementExprs.List[i]).IsConstant then
         Exit(False);
   Result:=True;
end;

// IsWritable
//
function TArrayConstantExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// TypeCheckElements
//
procedure TArrayConstantExpr.TypeCheckElements(prog : TdwsProgram);
var
   x : Integer;
   expr : TTypedExpr;
begin
   if FElementExprs.Count=0 then begin
      (FTyp as TStaticArraySymbol).Typ:=Prog.TypVariant;
      Exit;
   end;
   for x:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[x]);
      if (Typ.Typ=Prog.TypFloat) and (expr.Typ=Prog.TypInteger) then begin
         expr:=TConvFloatExpr.Create(Prog, expr);
         FElementExprs.List[x]:=expr;
      end;
      if not Typ.Typ.IsCompatible(expr.Typ) then
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_AssignIncompatibleTypes,
                                              [expr.Typ.Caption, Typ.Typ.Caption]);
   end;
end;

// ------------------
// ------------------ TRecordExpr ------------------
// ------------------

// Create
//
constructor TRecordExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                               BaseExpr: TDataExpr; MemberSymbol: TMemberSymbol);
begin
   inherited Create(Prog, Pos, MemberSymbol.Typ);
   FBaseExpr := BaseExpr;
   FMemberOffset := MemberSymbol.Offset;
end;

// Destroy
//
destructor TRecordExpr.Destroy;
begin
   FBaseExpr.Free;
   inherited;
end;

// GetAddr
//
function TRecordExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result := FBaseExpr.Addr[exec] + FMemberOffset;
end;

// GetData
//
function TRecordExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result := FBaseExpr.Data[exec];
end;

// GetSubExpr
//
function TRecordExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FBaseExpr;
end;

// GetSubExprCount
//
function TRecordExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// IsWritable
//
function TRecordExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
end;

// ------------------
// ------------------ TInitDataExpr ------------------
// ------------------

// Create
//
constructor TInitDataExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TDataExpr);
begin
   inherited Create(Prog, Pos);
   FExpr := Expr;
end;

// Destroy
//
destructor TInitDataExpr.Destroy;
begin
   FExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TInitDataExpr.EvalNoResult(exec : TdwsExecution);
begin
   FExpr.Typ.InitData(FExpr.Data[exec], FExpr.Addr[exec]);
end;

// GetSubExpr
//
function TInitDataExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExpr;
end;

// GetSubExprCount
//
function TInitDataExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TFieldExpr ------------------
// ------------------

// Create
//
constructor TFieldExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Typ: TTypeSymbol;
                              FieldSym: TFieldSymbol; ObjExpr: TDataExpr);
begin
   inherited Create(Prog, Pos, Typ);
   FObjectExpr := ObjExpr;
   FFieldAddr := FieldSym.Offset;
end;

// Destroy
//
destructor TFieldExpr.Destroy;
begin
   FObjectExpr.Free;
   inherited;
end;

// GetAddr
//
function TFieldExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result := FFieldAddr;
end;

// GetData
//
function TFieldExpr.GetData(exec : TdwsExecution) : TData;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.Data;
end;

// GetSubExpr
//
function TFieldExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FObjectExpr;
end;

// GetSubExprCount
//
function TFieldExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// Eval
//
function TFieldExpr.Eval(exec : TdwsExecution) : Variant;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.DataOfAddr(FFieldAddr);
end;

// EvalAsString
//
procedure TFieldExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.DataOfAddrAsString(FFieldAddr);
end;

// EvalAsInteger
//
function TFieldExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.DataOfAddrAsInteger(FFieldAddr);
end;

// EvalAsScriptObj
//
procedure TFieldExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   obj.DataOfAddrAsScriptObj(FFieldAddr, Result);
end;

// ------------------
// ------------------ TReadOnlyFieldExpr ------------------
// ------------------

// IsWritable
//
function TReadOnlyFieldExpr.IsWritable: Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TLazyParamExpr ------------------
// ------------------

// Create
//
constructor TLazyParamExpr.Create(Prog: TdwsProgram; aTyp : TTypeSymbol; level, stackAddr : Integer);
begin
   inherited Create(Prog);
   FTyp:=aTyp;
   FLevel:=level;
   FStackAddr:=stackAddr;
end;

// Eval
//
function TLazyParamExpr.Eval(exec : TdwsExecution) : Variant;
var
   lazyExpr : TExprBase;
   oldBasePointer: Integer;
   lazyContext : Int64;
begin
   lazyContext:=exec.Stack.ReadIntValue(exec.Stack.BasePointer + FStackAddr);
   lazyExpr:=TExprBase(lazyContext and $FFFFFFFF);

   oldBasePointer:=exec.Stack.BasePointer;
   exec.Stack.BasePointer:=(lazyContext shr 32);//  stack.GetSavedBp(Level);
   try
      lazyExpr.EvalAsVariant(exec, Result);
   finally
      exec.Stack.BasePointer:=oldBasePointer;
   end;
end;

// ------------------
// ------------------ TArrayLengthExpr ------------------
// ------------------

// EvalAsInteger
//
function TArrayLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   adr: Integer;
begin
   adr := TDataExpr(FExpr).Data[exec][TDataExpr(FExpr).Addr[exec]];
   Result := TDataExpr(FExpr).Data[exec][adr - 1] + FDelta;
end;

// ------------------
// ------------------ TOpenArrayLengthExpr ------------------
// ------------------

// EvalAsInteger
//
function TOpenArrayLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := Length(TDataExpr(FExpr).Data[exec])+FDelta;
end;

// ------------------
// ------------------ TStringArrayOpExpr ------------------
// ------------------

// CreatePos
//
constructor TStringArrayOpExpr.CreatePos(Prog: TdwsProgram; const Pos: TScriptPos;
                                         Left, Right: TTypedExpr);
begin
   inherited Create(Prog, Left, Right);
   FPos := Pos;
end;

// EvalAsString
//
procedure TStringArrayOpExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   i : Integer;
   buf : String;
begin
   FLeft.EvalAsString(exec, buf);
   i:=FRight.EvalAsInteger(exec);
   if i>Length(buf) then
      RaiseUpperExceeded(exec, i)
   else if i<1 then
      RaiseLowerExceeded(exec, i);
   Result:=buf[i];
end;

// ScriptPos
//
function TStringArrayOpExpr.ScriptPos : TScriptPos;
begin
   Result:=FPos;
end;

// ------------------
// ------------------ TStringLengthExpr ------------------
// ------------------

// EvalAsInteger
//
function TStringLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   buf : String;
begin
   FExpr.EvalAsString(exec, buf);
   Result:=Length(buf);
end;

// ------------------
// ------------------ TIsOpExpr ------------------
// ------------------

// EvalAsBoolean
//
function TIsOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   scriptObj : IScriptObj;
begin
   FLeft.EvalAsScriptObj(exec, scriptObj);
   Result:=Assigned(scriptObj) and FRight.Typ.Typ.IsCompatible(scriptObj.ClassSym);
end;

// ------------------
// ------------------ TAsOpExpr ------------------
// ------------------

// CreateAs
//
constructor TAsOpExpr.CreateAs(Prog: TdwsProgram; Left, Right: TTypedExpr; const aPos : TScriptPos);
begin
   inherited Create(prog, left, right);
   FPos:=aPos;
   FTyp:=right.Typ.Typ;
end;

// Eval
//
function TAsOpExpr.Eval(exec : TdwsExecution) : Variant;
var
   scriptObj: IScriptObj;
begin
   EvalAsScriptObj(exec, scriptObj);
   Result:=scriptObj;
end;

// EvalAsScriptObj
//
procedure TAsOpExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   procedure RaiseClassCastFailed;
   begin
      RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_ClassCastFailed,
                                                     [Result.ClassSym.Caption, FTyp.Caption]))
   end;

begin
   FLeft.EvalAsScriptObj(exec, Result);

   if Assigned(Result) and not (FTyp.IsCompatible(Result.ClassSym)) then
      RaiseClassCastFailed;
end;

// ------------------
// ------------------ TInOpExpr ------------------
// ------------------

// Create
//
constructor TInOpExpr.Create(Prog: TdwsProgram; Left: TTypedExpr);
begin
   inherited Create(Prog);
   FLeft:=Left;
   FTyp:=Prog.TypBoolean;
end;

// Destroy
//
destructor TInOpExpr.Destroy;
begin
   FLeft.Free;
   FCaseConditions.Clean;
   inherited;
end;

// Eval
//
function TInOpExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsBoolean(exec);
end;

// EvalAsBoolean
//
function TInOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   i : Integer;
   value : Variant;
   cc : TCaseCondition;
begin
   FLeft.EvalAsVariant(exec, value);
   for i:=0 to FCaseConditions.Count-1 do begin
      cc:=TCaseCondition(FCaseConditions.List[i]);
      if cc.IsTrue(exec, Value) then
         Exit(True);
   end;
   Result:=False;
end;

// IsConstant
//
function TInOpExpr.IsConstant : Boolean;
var
   i : Integer;
begin
   Result:=FLeft.IsConstant;
   if Result then
      for i:=0 to FCaseConditions.Count-1 do
         if not TCaseCondition(FCaseConditions.List[i]).IsConstant then
            Exit(False);
end;

// AddCaseCondition
//
procedure TInOpExpr.AddCaseCondition(cond : TCaseCondition);
begin
   FCaseConditions.Add(cond);
end;

// ------------------
// ------------------ TConvExpr ------------------
// ------------------

// WrapWithConvCast
//
class function TConvExpr.WrapWithConvCast(prog : TdwsProgram; const scriptPos : TScriptPos;
                                          toTyp : TTypeSymbol; expr : TTypedExpr;
                                          reportError : Boolean) : TTypedExpr;

   procedure ReportIncompatibleTypes;
   var
      cleft, cright: string;
   begin
      if not reportError then Exit;
      if toTyp = nil then
         cleft := SYS_VOID
      else cleft := toTyp.Caption;
      if expr.Typ = nil then
         cright := SYS_VOID
      else cright := expr.Typ.Caption;
      prog.CompileMsgs.AddCompilerErrorFmt(scriptPos, CPE_AssignIncompatibleTypes, [cright, cleft]);
   end;

begin
   Result:=expr;
   if (toTyp=nil) or (expr.Typ=nil) then begin
      ReportIncompatibleTypes;
      Exit;
   end;

   if expr.IsOfType(prog.TypVariant) then begin
      if toTyp.IsOfType(prog.TypInteger) then
         Result:=TConvIntegerExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypFloat) then
         Result:=TConvFloatExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypString) then
         Result:=TConvStringExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypBoolean) then
         Result:=TConvBoolExpr.Create(prog, expr);
   end else begin
      if toTyp.IsOfType(prog.TypFloat) then
         if expr.IsOfType(prog.TypInteger) then
            Result:=TConvFloatExpr.Create(prog, expr);
   end;
   // Look if Types are compatible
   if not toTyp.IsCompatible(Result.Typ) then
      ReportIncompatibleTypes;
end;

// ------------------
// ------------------ TConvFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TConvFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FExpr.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TConvIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TConvIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TConvStringExpr ------------------
// ------------------

// EvalAsString
//
procedure TConvStringExpr.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   FExpr.EvalAsString(exec, Result);
end;

// ------------------
// ------------------ TConvBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FExpr.EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TConvVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TConvVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   FExpr.EvalAsVariant(exec, Result);
end;

// ------------------
// ------------------ TConvClassExpr ------------------
// ------------------

// Eval
//
function TConvClassExpr.Eval(exec : TdwsExecution) : Variant;
var
   obj : IScriptObj;
begin
   FExpr.EvalAsScriptObj(exec, obj);
   if (obj<>nil) and (not obj.ClassSym.IsOfType(Typ)) then
      raise EClassCast.CreateFmt(RTE_ClassCastFailed, [obj.ClassSym.Name, Typ.Name]);
   Result:=obj;
end;

// ------------------
// ------------------ TAssertExpr ------------------
// ------------------

// Create
//
constructor TAssertExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; condExpr, msgExpr : TTypedExpr);
begin
   inherited Create(Prog, Pos);
   FCond:=condExpr;
   FMessage:=msgExpr;
end;

// Destroy
//
destructor TAssertExpr.Destroy;
begin
   FCond.Free;
   FMessage.Free;
   inherited;
end;

// EvalNoResult
//
procedure TAssertExpr.EvalNoResult(exec : TdwsExecution);

   procedure Triggered;
   var
      msg : String;
   begin
      if FMessage<>nil then begin
         FMessage.EvalAsString(exec, msg);
         msg:=' : '+msg;
      end else msg:='';
      (exec as TdwsProgramExecution).RaiseAssertionFailed(msg, FScriptPos);
   end;

begin
   if not FCond.EvalAsBoolean(exec) then
      Triggered;
end;

// Optimize
//
function TAssertExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCond.IsConstant and (not FCond.EvalAsBoolean(exec)) then begin
      Result:=TNullExpr.Create(Prog, FScriptPos);
      Free;
   end;
end;

// GetSubExpr
//
function TAssertExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FCond;
      1 : Result:=FMessage;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TAssertExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TAssignedInstanceExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssignedInstanceExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   obj : IScriptObj;
begin
   FExpr.EvalAsScriptObj(exec, obj);
   Result:=(obj<>nil);
end;

// ------------------
// ------------------ TAssignedMetaClassExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssignedMetaClassExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FExpr.EvalAsInteger(exec)<>0);
end;

// ------------------
// ------------------ TOrdExpr ------------------
// ------------------

// EvalAsInteger
//
function TOrdExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   v : Variant;
   s : String;
begin
   Result:=0;
   FExpr.EvalAsVariant(exec, v);
   case VarType(v) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64 :
         Result:=v;
      varBoolean :
         if v then
            Result:=1
         else Result:=0;
      varSingle, varDouble, varCurrency :
         Result:=Round(v);
      varString, varUString, varOleStr : begin
         s:=v;
         if s<>'' then
            Result:=Ord(s[1]);
      end;
   else
      RaiseScriptError(exec, EScriptError.Create(RTE_OrdinalExpected));
   end;
end;

{ TOrdIntExpr }

// EvalAsInteger
//
function TOrdIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FExpr.EvalAsInteger(exec);
end;

{ TOrdBoolExpr }

// EvalAsInteger
//
function TOrdBoolExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Ord(FExpr.EvalAsBoolean(exec));
end;

{ TOrdStrExpr }

// EvalAsInteger
//
function TOrdStrExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   s : String;
begin
   FExpr.EvalAsString(exec, s);
   if s<>'' then
      Result:=Ord(s[1])
   else Result:=0;
end;

// ------------------
// ------------------ TObjCmpExpr ------------------
// ------------------

// EvalAsBoolean
//
function TObjCmpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   iLeft, iRight : IScriptObj;
begin
   FLeft.EvalAsScriptObj(exec, iLeft);
   FRight.EvalAsScriptObj(exec, iRight);
   Result:=(iLeft=iRight);
end;

// ------------------
// ------------------ TNegVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TNegVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Expr.EvalAsVariant(exec, Result);
   Result:=-Result;
end;

// ------------------
// ------------------ TNegIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TNegIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=-FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TNegFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TNegFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=-FExpr.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TAddVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TAddVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=lv+rv;
end;

// ------------------
// ------------------ TAddStrExpr ------------------
// ------------------

// EvalAsString
//
procedure TAddStrExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   buf : String;
begin
   FLeft.EvalAsString(exec, Result);
   FRight.EvalAsString(exec, buf);
   Result:=Result+buf;
end;

// ------------------
// ------------------ TAddIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TAddIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec)+FRight.EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TAddIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsInteger(exec)+FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TAddFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TAddFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)+FRight.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TSubVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TSubVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=lv-rv;
end;

// ------------------
// ------------------ TSubIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TSubIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec)-FRight.EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TSubIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsInteger(exec)-FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TSubFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TSubFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)-FRight.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TMultVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TMultVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=lv*rv;
end;

// ------------------
// ------------------ TMultIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TMultIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec)*FRight.EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TMultIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsInteger(exec)*FRight.EvalAsInteger(exec);
end;

// Optimize
//
function TMultIntExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if (FLeft is TVarExpr) and (FRight is TVarExpr) then begin
      if TVarExpr(FLeft).SameVarAs(TVarExpr(FRight)) then begin
         Result:=TSqrIntExpr.Create(Prog, FLeft);
         FLeft:=nil;
      end;
   end;
end;

// ------------------
// ------------------ TMultFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TMultFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)*FRight.EvalAsFloat(exec);
end;

// Optimize
//
function TMultFloatExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if (FLeft is TFloatVarExpr) and (FRight is TFloatVarExpr) then begin
      if TFloatVarExpr(FLeft).SameVarAs(TFloatVarExpr(FRight)) then begin
         Result:=TSqrFloatExpr.Create(Prog, FLeft);
         FLeft:=nil;
         Free;
      end;
   end;
end;

// ------------------
// ------------------ TSqrIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TSqrIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Sqr(FExpr.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TSqrFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TSqrFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$ifdef PUREPASCAL}
begin
   Result:=Sqr(FExpr.EvalAsFloat(exec));
{$else}
asm
   mov   eax, [eax].FExpr
   mov   ecx, [eax]
   call  [ecx+VMTOFFSET EvalAsFloat]
   fmul  st(0), st(0)
{$endif}
end;

// ------------------
// ------------------ TDivideExpr ------------------
// ------------------

// EvalAsFloat
//
function TDivideExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)/FRight.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TDivExpr ------------------
// ------------------

// TDivExpr
//
function TDivExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec) div FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TModExpr ------------------
// ------------------

// EvalAsInteger
//
function TModExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec) mod FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TNotBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TNotBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=not FExpr.EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TNotIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TNotIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=not FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TNotVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TNotVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   FExpr.EvalAsVariant(exec, Result);
   Result:=not Result;
end;

{ TIntAndExpr }

function TIntAndExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) and FRight.EvalAsInteger(exec);
end;

{ TBoolAndExpr }

function TBoolAndExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBoolean(exec) and Right.EvalAsBoolean(exec);
end;

{ TIntOrExpr }

function TIntOrExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) or FRight.EvalAsInteger(exec);
end;

{ TBoolOrExpr }

function TBoolOrExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBoolean(exec) or Right.EvalAsBoolean(exec);
end;

{ TIntXorExpr }

function TIntXorExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) xor FRight.EvalAsInteger(exec);
end;

{ TBoolXorExpr }

function TBoolXorExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := FLeft.EvalAsBoolean(exec) xor FRight.EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TBoolImpliesExpr ------------------
// ------------------

// EvalAsBoolean
//
function TBoolImpliesExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(not FLeft.EvalAsBoolean(exec)) or FRight.EvalAsBoolean(exec);;
end;

// ------------------
// ------------------ TShlExpr ------------------
// ------------------

// EvalAsInteger
//
function TShlExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) shl FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TShrExpr ------------------
// ------------------

// EvalAsInteger
//
function TShrExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) shr FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TAssignExpr ------------------
// ------------------

constructor TAssignExpr.Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr);
begin
  inherited Create(Prog, Pos);
  FLeft := Left;
  FRight := Right;
  TypeCheckAssign(Prog);
end;

// Destroy
//
destructor TAssignExpr.Destroy;
begin
   FLeft.Free;
   FRight.Free;
   inherited;
end;

// EvalNoResult
//
procedure TAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignExpr(exec, FRight);
end;

// TypeCheckAssign
//
procedure TAssignExpr.TypeCheckAssign(prog : TdwsProgram);
begin
   if FRight.ClassType=TArrayConstantExpr then
      TArrayConstantExpr(FRight).Prepare(Prog, FLeft.Typ.Typ);

   FRight:=TConvExpr.WrapWithConvCast(prog, ScriptPos, FLeft.Typ, FRight, True);
end;

// Optimize
//
function TAssignExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   leftVarExpr : TVarExpr;
   addIntExpr : TAddIntExpr;
   addStrExpr : TAddStrExpr;
   subIntExpr : TSubIntExpr;
begin
   if FRight.IsConstant then
      Exit(OptimizeConstAssignment(prog, exec));

   Result:=Self;
   if FLeft.InheritsFrom(TVarExpr) then begin
      leftVarExpr:=TVarExpr(FLeft);
      if leftVarExpr.ClassType=TIntVarExpr then begin
         if FRight.InheritsFrom(TAddIntExpr) then begin
            addIntExpr:=TAddIntExpr(FRight);
            if (addIntExpr.Left is TVarExpr) and (TVarExpr(addIntExpr.Left).SameVarAs(leftVarExpr)) then begin
               Result:=TIncIntVarExpr.Create(Prog, FScriptPos, FLeft, addIntExpr.Right);
               FLeft:=nil;
               addIntExpr.Right:=nil;
               Free;
            end;
         end else if FRight.InheritsFrom(TSubIntExpr) then begin
            subIntExpr:=TSubIntExpr(FRight);
            if (subIntExpr.Left is TVarExpr) and (TVarExpr(subIntExpr.Left).SameVarAs(leftVarExpr)) then begin
               Result:=TDecIntVarExpr.Create(Prog, FScriptPos, FLeft, subIntExpr.Right);
               FLeft:=nil;
               subIntExpr.Right:=nil;
               Free;
            end;
         end;
      end else if leftVarExpr.ClassType=TStrVarExpr then begin
         if FRight.InheritsFrom(TAddStrExpr) then begin
            addStrExpr:=TAddStrExpr(FRight);
            if (addStrExpr.Left is TVarExpr) and (TVarExpr(addStrExpr.Left).SameVarAs(leftVarExpr)) then begin
               if addStrExpr.Right.InheritsFrom(TConstStringExpr) then begin
                  Result:=TAppendConstStringVarExpr.Create(Prog, FScriptPos, FLeft, addStrExpr.Right);
               end else begin
                  Result:=TAppendStringVarExpr.Create(Prog, FScriptPos, FLeft, addStrExpr.Right);
               end;
               FLeft:=nil;
               addStrExpr.Right:=nil;
               Free;
            end;
         end;
      end;
   end;
end;

// OptimizeConstAssignment
//
function TAssignExpr.OptimizeConstAssignment(prog : TdwsProgram; exec : TdwsExecution) : TNoResultExpr;
var
   stringBuf : String;
begin
   Result:=Self;
   if FRight.IsOfType(prog.TypInteger) then begin

      Result:=TAssignConstToIntegerVarExpr.CreateVal(prog, FScriptPos, FLeft, FRight.EvalAsInteger(exec));

   end else if FRight.IsOfType(prog.TypFloat) then begin

      Result:=TAssignConstToFloatVarExpr.CreateVal(prog, FScriptPos, FLeft, FRight.EvalAsFloat(exec));

   end else if FRight.IsOfType(prog.TypString) then begin

      FRight.EvalAsString(exec, stringBuf);
      Result:=TAssignConstToStringVarExpr.CreateVal(prog, FScriptPos, FLeft, stringBuf);

   end;
   if Result<>Self then begin
      FLeft:=nil;
      Free;
   end;
end;

// GetSubExpr
//
function TAssignExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FLeft;
      1 : Result:=FRight;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TAssignExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TAssignClassOfExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssignClassOfExpr.EvalNoResult(exec : TdwsExecution);
var
   v : Variant;
   obj : IScriptObj;
begin
   FRight.EvalAsVariant(exec, v);
   if VarIsOrdinal(v) then
      FLeft.AssignValue(exec, v)
   else begin
      obj:=IScriptObj(IUnknown(v));
      if obj<>nil then
         FLeft.AssignValueAsInteger(exec, Int64(IScriptObj(IUnknown(v)).ClassSym))
      else FLeft.AssignValueAsInteger(exec, 0);
   end;
end;

{ TAssignDataExpr }

constructor TAssignDataExpr.Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr);
begin
  inherited Create(Prog, Pos, Left, Right);
  FSize := FLeft.Typ.Size;
end;

procedure TAssignDataExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignDataExpr(exec, TDataExpr(FRight));
end;

// ------------------
// ------------------ TAssignFuncExpr ------------------
// ------------------

// TypeCheckAssign
//
procedure TAssignFuncExpr.TypeCheckAssign(prog : TdwsProgram);
begin
   if not (FRight as TFuncExprBase).FuncSym.IsCompatible(FLeft.Typ) then
      prog.CompileMsgs.AddCompilerError(ScriptPos, CPE_IncompatibleOperands);
end;

// EvalNoResult
//
procedure TAssignFuncExpr.EvalNoResult(exec : TdwsExecution);
var
   funcPtr : TFuncPointer;
   funcExpr : TFuncExprBase;
begin
   funcExpr:=(FRight as TFuncExprBase);
   funcPtr:=TFuncPointer.Create(exec, funcExpr);
   FLeft.AssignValue(exec, IFuncPointer(funcPtr));
end;

// Optimize
//
function TAssignFuncExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TAssignArrayConstantExpr ------------------
// ------------------

constructor TAssignArrayConstantExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                                            Left : TDataExpr; Right: TTypedExpr);
begin
  inherited Create(Prog, Pos, Left, Right as TArrayConstantExpr); // typecheck Right
end;

procedure TAssignArrayConstantExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignData(exec, TArrayConstantExpr(FRight).EvalAsTData(exec), 0);
end;

// ------------------
// ------------------ TAssignConstDataToVarExpr ------------------
// ------------------

constructor TAssignConstDataToVarExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                                             Left : TDataExpr; Right: TTypedExpr);
begin
   inherited Create(Prog, Pos, Left, Right);
   Assert(Left is TVarExpr);
   if Right=nil then
      Assert(ClassType<>TAssignConstDataToVarExpr)
   else Assert(Right is TConstExpr);
end;

procedure TAssignConstDataToVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignData(exec, TConstExpr(FRight).FData, TConstExpr(FRight).Addr[exec]);
end;

// ------------------
// ------------------ TAssignConstExpr ------------------
// ------------------

// TypeCheckAssign
//
procedure TAssignConstExpr.TypeCheckAssign(prog : TdwsProgram);
begin
   // nothing, checked during optimize
end;

// ------------------
// ------------------ TAssignConstToIntegerVarExpr ------------------
// ------------------

// Create
//
constructor TAssignConstToIntegerVarExpr.CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Int64);
begin
   inherited Create(Prog, Pos, Left, nil);
   FRight:=rightValue;
end;

// EvalNoResult
//
procedure TAssignConstToIntegerVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsInteger(exec, FRight);
end;

// ------------------
// ------------------ TAssignConstToFloatVarExpr ------------------
// ------------------

// Create
//
constructor TAssignConstToFloatVarExpr.CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Double);
begin
   inherited Create(Prog, Pos, Left, nil);
   FRight:=rightValue;
end;

// EvalNoResult
//
procedure TAssignConstToFloatVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsFloat(exec, FRight);
end;

// ------------------
// ------------------ TAssignConstToStringVarExpr ------------------
// ------------------

// Create
//
constructor TAssignConstToStringVarExpr.CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : String);
begin
   inherited Create(Prog, Pos, Left, nil);
   FRight:=rightValue;
end;

// EvalNoResult
//
procedure TAssignConstToStringVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsString(exec, FRight);
end;

// ------------------
// ------------------ TOpAssignExpr ------------------
// ------------------

// Optimize
//
function TOpAssignExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TPlusAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   FLeft.AssignValue(exec, lv+rv);
end;

// ------------------
// ------------------ TPlusAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignIntExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsInteger(exec, FLeft.EvalAsInteger(exec) + FRight.EvalAsInteger(exec));
end;

// Optimize
//
function TPlusAssignIntExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FLeft is TIntVarExpr then begin
      Result:=TIncIntVarExpr.Create(Prog, FScriptPos, FLeft, FRight);
      FLeft:=nil;
      FRight:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TPlusAssignFloatExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignFloatExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)+FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TPlusAssignStrExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignStrExpr.EvalNoResult(exec : TdwsExecution);
var
   v1, v2 : String;
begin
   FLeft.EvalAsString(exec, v1);
   FRight.EvalAsString(exec, v2);
   FLeft.AssignValueAsString(exec, v1+v2);
end;

// Optimize
//
function TPlusAssignStrExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FLeft is TStrVarExpr then begin
      Result:=TAppendStringVarExpr.Create(Prog, FScriptPos, FLeft, FRight);
      FLeft:=nil;
      FRight:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TMinusAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMinusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   FLeft.AssignValue(exec, lv-rv);
end;

// ------------------
// ------------------ TMinusAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMinusAssignIntExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsInteger(exec, FLeft.EvalAsInteger(exec) - FRight.EvalAsInteger(exec));
end;

// Optimize
//
function TMinusAssignIntExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FLeft is TIntVarExpr then begin
      Result:=TDecIntVarExpr.Create(Prog, FScriptPos, FLeft, FRight);
      FLeft:=nil;
      FRight:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TMinusAssignFloatExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMinusAssignFloatExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)-FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TMultAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   FLeft.AssignValue(exec, lv*rv);
end;

// ------------------
// ------------------ TMultAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignIntExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsInteger(exec, FLeft.EvalAsInteger(exec) * FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TMultAssignFloatExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignFloatExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)*FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TDivideAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDivideAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)/FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TIncIntVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TIncIntVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TIntVarExpr(FLeft).IncValue(exec, FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TDecIntVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDecIntVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TIntVarExpr(FLeft).IncValue(exec, -FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TAppendStringVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAppendStringVarExpr.EvalNoResult(exec : TdwsExecution);
var
   buf : String;
begin
   FRight.EvalAsString(exec, buf);
   TStrVarExpr(FLeft).Append(exec, buf);
end;

// ------------------
// ------------------ TAppendConstStringVarExpr ------------------
// ------------------

// Create
//
constructor TAppendConstStringVarExpr.Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr);
begin
   inherited Create(Prog, Pos, Left, Right);
   FAppendString:=(right as TConstStringExpr).FValue;
end;

// EvalNoResult
//
procedure TAppendConstStringVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TStrVarExpr(FLeft).Append(exec, FAppendString);
end;

// ------------------
// ------------------ TBlockExpr ------------------
// ------------------

// Create
//
constructor TBlockExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos);
begin
   inherited Create(Prog, Pos);
   FTable:=TSymbolTable.Create(Prog.Table, Prog.Table.AddrGenerator);
end;

// Destroy
//
destructor TBlockExpr.Destroy;
begin
   FTable.Free;
   inherited;
end;

// EvalNoResult
//
procedure TBlockExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   oldTable : TSymbolTable;
   expr : PNoResultExpr;
begin
   oldTable:=exec.ContextTable;
   try
      exec.ContextTable:=FTable;
      expr:=@FStatements[0];
      for i:=1 to FCount do begin
         exec.DoStep(expr^);
         expr.EvalNoResult(exec);
         if exec.Status<>esrNone then Break;
         Inc(expr);
      end;
   finally
      exec.ContextTable:=oldTable;
   end;
end;

// Optimize
//
function TBlockExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   i : Integer;
begin
   for i:=FCount-1 downto 0 do begin
      if FStatements[i].ClassType=TNullExpr then begin
         FStatements[i].Free;
         if i+1<FCount then
            Move(FStatements[i+1], FStatements[i], SizeOf(TNoResultExpr)*(FCount-1-i));
         Dec(FCount);
         ReallocMem(FStatements, FCount*SizeOf(TNoResultExpr));
      end;
   end;

   if FTable.Count=0 then begin
      case FCount of
         0 : Result:=TNullExpr.Create(Prog, FScriptPos);
         1 : begin
            Result:=FStatements[0];
            FreeMem(FStatements);
         end;
      else
         case FCount of
            2 : Result:=TBlockExprNoTable2.Create(Prog, FScriptPos);
            3 : Result:=TBlockExprNoTable3.Create(Prog, FScriptPos);
            4 : Result:=TBlockExprNoTable4.Create(Prog, FScriptPos);
         else
            Result:=TBlockExprNoTable.Create(Prog, FScriptPos);
         end;
         TBlockExprNoTable(Result).FStatements:=FStatements;
         TBlockExprNoTable(Result).FCount:=FCount;
      end;
      FStatements:=nil;
      FCount:=0;
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TBlockExprNoTable ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   iterator : PNoResultExpr;
begin
   iterator:=PNoResultExpr(FStatements);
   for i:=1 to FCount do begin
      exec.DoStep(iterator^);
      iterator^.EvalNoResult(exec);
      if exec.Status<>esrNone then Break;
      Inc(iterator);
   end;
end;

// ------------------
// ------------------ TBlockExprNoTable2 ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable2.EvalNoResult(exec : TdwsExecution);
var
   statements : PNoResultExprList;
begin
   statements:=FStatements;
   exec.DoStep(statements[0]);
   statements[0].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[1]);
   statements[1].EvalNoResult(exec);
end;

// ------------------
// ------------------ TBlockExprNoTable3 ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable3.EvalNoResult(exec : TdwsExecution);
var
   statements : PNoResultExprList;
begin
   statements:=FStatements;
   exec.DoStep(statements[0]);
   statements[0].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[1]);
   statements[1].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[2]);
   statements[2].EvalNoResult(exec);
end;

// ------------------
// ------------------ TBlockExprNoTable4 ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable4.EvalNoResult(exec : TdwsExecution);
var
   statements : PNoResultExprList;
begin
   statements:=FStatements;
   exec.DoStep(statements[0]);
   statements[0].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[1]);
   statements[1].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[2]);
   statements[2].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[3]);
   statements[3].EvalNoResult(exec);
end;

// ------------------
// ------------------ TIfThenExpr ------------------
// ------------------

// Create
//
constructor TIfThenExpr.Create(prog : TdwsProgram; const Pos : TScriptPos;
                               condExpr : TTypedExpr; thenExpr : TNoResultExpr);
begin
   inherited Create(prog, pos);
   FCond:=condExpr;
   FThen:=thenExpr;
end;

// Destroy
//
destructor TIfThenExpr.Destroy;
begin
   FCond.Free;
   FThen.Free;
   inherited;
end;

// EvalNoResult
//
procedure TIfThenExpr.EvalNoResult(exec : TdwsExecution);
begin
   if FCond.EvalAsBoolean(exec) then begin
      exec.DoStep(FThen);
      FThen.EvalNoResult(exec);
   end;
end;

// Optimize
//
function TIfThenExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCond.IsConstant then begin
      if FCond.EvalAsBoolean(exec) then begin
         Result:=FThen;
         FThen:=nil;
      end else Result:=TNullExpr.Create(Prog, FScriptPos);
      Free;
   end;
end;

// GetSubExpr
//
function TIfThenExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FCond;
      1 : Result:=FThen;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TIfThenExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TIfThenElseExpr ------------------
// ------------------

// Create
//
constructor TIfThenElseExpr.Create(prog : TdwsProgram; const Pos : TScriptPos;
                 condExpr : TTypedExpr; thenExpr, elseExpr : TNoResultExpr);
begin
   inherited Create(prog, pos, condExpr, thenExpr);
   FElse:=elseExpr;
end;

// Destroy
//
destructor TIfThenElseExpr.Destroy;
begin
   FElse.Free;
   inherited;
end;

// EvalNoResult
//
procedure TIfThenElseExpr.EvalNoResult(exec : TdwsExecution);
begin
   if FCond.EvalAsBoolean(exec) then begin
      exec.DoStep(FThen);
      FThen.EvalNoResult(exec);
   end else begin
      exec.DoStep(FElse);
      FElse.EvalNoResult(exec);
   end;
end;

// Optimize
//
function TIfThenElseExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCond.IsConstant then begin
      if FCond.EvalAsBoolean(exec) then begin
         Result:=FThen;
         FThen:=nil;
      end else begin
         Result:=FElse;
         FElse:=nil;
      end;
      if Result=nil then
         Result:=TNullExpr.Create(Prog, FScriptPos);
      Free;
   end;
end;

// GetSubExpr
//
function TIfThenElseExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=2 then
      Result:=FElse
   else Result:=inherited GetSubExpr(i);
end;

// GetSubExprCount
//
function TIfThenElseExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

{ TCaseExpr }

destructor TCaseExpr.Destroy;
begin
  FCaseConditions.Clean;
  FValueExpr.Free;
  FElseExpr.Free;
  inherited;
end;

// EvalNoResult
//
procedure TCaseExpr.EvalNoResult(exec : TdwsExecution);
var
   x : Integer;
   value: Variant;
   cc : TCaseCondition;
begin
   FValueExpr.EvalAsVariant(exec, value);
   for x := 0 to FCaseConditions.Count - 1 do begin
      cc:=TCaseCondition(FCaseConditions.List[x]);
      if cc.IsTrue(exec, value) then begin
         exec.DoStep(cc.TrueExpr);
         cc.TrueExpr.EvalNoResult(exec);
         Exit;
      end;
   end;

   if Assigned(FElseExpr) then begin
      exec.DoStep(FElseExpr);
      FElseExpr.EvalNoResult(exec);
   end;
end;

// AddCaseCondition
//
procedure TCaseExpr.AddCaseCondition(cond : TCaseCondition);
begin
   FCaseConditions.Add(cond);
end;

// ------------------
// ------------------ TCaseCondition ------------------
// ------------------

// Create
//
constructor TCaseCondition.Create(const aPos : TScriptPos);
begin
   FPos:=aPos;
end;

// Destroy
//
destructor TCaseCondition.Destroy;
begin
   if FOwnsTrueExpr then
      FTrueExpr.Free;
   inherited;
end;

// IsOfTypeNumber
//
function TCaseCondition.IsOfTypeNumber(prog : TdwsProgram; typ : TTypeSymbol) : Boolean;
begin
   Result:=   typ.IsOfType(prog.TypInteger) or typ.IsOfType(prog.TypFloat)
           or typ.IsOfType(prog.TypVariant)
           or (typ is TEnumerationSymbol);
end;

// ------------------
// ------------------ TCompareCaseCondition ------------------
// ------------------

// Create
//
constructor TCompareCaseCondition.Create(const aPos : TScriptPos; compareExpr : TTypedExpr);
begin
   inherited Create(aPos);
   FCompareExpr:=compareExpr;
end;

// Destroy
//
destructor TCompareCaseCondition.Destroy;
begin
   FCompareExpr.Free;
   inherited;
end;

// IsTrue
//
function TCompareCaseCondition.IsTrue(exec : TdwsExecution; const value : Variant) : Boolean;
var
   buf : Variant;
begin
   FCompareExpr.EvalAsVariant(exec, buf);
   Result:=(buf=Value);
end;

// TypeCheck
//
procedure TCompareCaseCondition.TypeCheck(prog : TdwsProgram; typ : TTypeSymbol);
begin
   if not FCompareExpr.Typ.IsCompatible(typ) then
      if not (IsOfTypeNumber(prog, FCompareExpr.Typ) and IsOfTypeNumber(prog, typ)) then
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                              [typ.Caption, FCompareExpr.Typ.Caption]);
end;

// IsConstant
//
function TCompareCaseCondition.IsConstant : Boolean;
begin
   Result:=FCompareExpr.IsConstant;
end;

// ------------------
// ------------------ TRangeCaseCondition ------------------
// ------------------

// Create
//
constructor TRangeCaseCondition.Create(const aPos : TScriptPos; fromExpr, toExpr : TTypedExpr);
begin
   inherited Create(aPos);
   FFromExpr:=FromExpr;
   FToExpr:=ToExpr;
end;

// Destroy
//
destructor TRangeCaseCondition.Destroy;
begin
   FFromExpr.Free;
   FToExpr.Free;
   inherited;
end;

// IsTrue
//
function TRangeCaseCondition.IsTrue(exec : TdwsExecution; const value : Variant) : Boolean;
var
   v : Variant;
begin
   FFromExpr.EvalAsVariant(exec, v);
   if value>=v then begin
      FToExpr.EvalAsVariant(exec, v);
      Result:=(value<=v);
   end else Result:=False;
end;

// TypeCheck
//
procedure TRangeCaseCondition.TypeCheck(prog : TdwsProgram; typ : TTypeSymbol);
var
   fromIsNumber : Boolean;
begin
   fromIsNumber:=IsOfTypeNumber(prog, FFromExpr.Typ);

   if not FFromExpr.Typ.IsCompatible(FToExpr.Typ) then begin
      if not (fromIsNumber and IsOfTypeNumber(prog, FToExpr.Typ)) then begin
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_RangeIncompatibleTypes,
                                              [FFromExpr.Typ.Caption, FToExpr.Typ.Caption]);
         Exit;
      end;
   end;

   if not typ.IsCompatible(FFromExpr.Typ) then
      if not (fromIsNumber and IsOfTypeNumber(prog, typ)) then
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                              [typ.Caption, FFromExpr.Typ.Caption]);
end;

// IsConstant
//
function TRangeCaseCondition.IsConstant : Boolean;
begin
   Result:=FFromExpr.IsConstant and FToExpr.IsConstant;
end;

// ------------------
// ------------------ TForExpr ------------------
// ------------------

// Destroy
//
destructor TForExpr.Destroy;
begin
   FDoExpr.Free;
   FFromExpr.Free;
   FToExpr.Free;
   FVarExpr.Free;
   inherited;
end;

// GetSubExpr
//
function TForExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FVarExpr;
      1 : Result:=FFromExpr;
      2 : Result:=FToExpr;
      3 : Result:=FDoExpr;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TForExpr.GetSubExprCount : Integer;
begin
   Result:=4;
end;

// ------------------
// ------------------ TForStepExpr ------------------
// ------------------

// Destroy
//
destructor TForStepExpr.Destroy;
begin
   FStepExpr.Free;
   inherited;
end;

// EvalStep
//
function TForStepExpr.EvalStep(exec : TdwsExecution) : Int64;
begin
   Result:=FStepExpr.EvalAsInteger(exec);
   if Result<=0 then
      RaiseScriptError(exec, EScriptError.CreateFmt(RTE_ForLoopStepShouldBeStrictlyPositive, [Result]));
end;

// GetSubExpr
//
function TForStepExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=4 then
      Result:=FStepExpr
   else Result:=inherited GetSubExpr(i);
end;

// GetSubExprCount
//
function TForStepExpr.GetSubExprCount : Integer;
begin
   Result:=5;
end;

// ------------------
// ------------------ TForUpwardExpr ------------------
// ------------------

procedure TForUpwardExpr.EvalNoResult(exec : TdwsExecution);
var
   toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   while i^<=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      Inc(i^);
   end;
end;

// ------------------
// ------------------ TForDownwardExpr ------------------
// ------------------

procedure TForDownwardExpr.EvalNoResult(exec : TdwsExecution);
var
   toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   while i^>=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      Dec(i^);
   end;
end;

// ------------------
// ------------------ TForUpwardStepExpr ------------------
// ------------------

procedure TForUpwardStepExpr.EvalNoResult(exec : TdwsExecution);
var
   step, toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   step:=EvalStep(exec);
   while i^<=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      try
         {$OVERFLOWCHECKS ON}
         i^:=i^+step;
         {$OVERFLOWCHECKS OFF}
      except
         Break;
      end;
   end;
end;

// ------------------
// ------------------ TForDownwardStepExpr ------------------
// ------------------

procedure TForDownwardStepExpr.EvalNoResult(exec : TdwsExecution);
var
   step, toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   step:=EvalStep(exec);
   while i^>=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      try
         {$OVERFLOWCHECKS ON}
         i^:=i^-step;
         {$OVERFLOWCHECKS OFF}
      except
         Break;
      end;
   end;
end;

// ------------------
// ------------------ TLoopExpr ------------------
// ------------------

// Destroy
//
destructor TLoopExpr.Destroy;
begin
   FCondExpr.Free;
   FLoopExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TLoopExpr.EvalNoResult(exec : TdwsExecution);
begin
   repeat
      exec.DoStep(FLoopExpr);
      FLoopExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               Break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
   until False;
end;

// GetSubExpr
//
function TLoopExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FCondExpr;
      1 : Result:=FLoopExpr;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TLoopExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

{ TWhileExpr }

procedure TWhileExpr.EvalNoResult(exec : TdwsExecution);
begin
   while FCondExpr.EvalAsBoolean(exec) do begin
      exec.DoStep(FLoopExpr);
      FLoopExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               Break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
   end;
end;

// Optimize
//
function TWhileExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCondExpr.IsConstant then begin
      if not FCondExpr.EvalAsBoolean(exec) then begin
         Result:=TNullExpr.Create(prog, FScriptPos);
      end else begin
         Result:=TLoopExpr.Create(prog, FScriptPos);
         TLoopExpr(Result).FLoopExpr:=FLoopExpr;
         FLoopExpr:=nil;
      end;
      Free;
   end;
end;

{ TRepeatExpr }

procedure TRepeatExpr.EvalNoResult(exec : TdwsExecution);
begin
   repeat
      exec.DoStep(FLoopExpr);
      FLoopExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               Break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      exec.DoStep(Self);
   until FCondExpr.EvalAsBoolean(exec);
end;

// Optimize
//
function TRepeatExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCondExpr.IsConstant and not FCondExpr.EvalAsBoolean(exec) then begin
      Result:=TLoopExpr.Create(prog, FScriptPos);
      TLoopExpr(Result).FLoopExpr:=FLoopExpr;
      FLoopExpr:=nil;
      Free;
   end;
end;

{ TBreakExpr }

procedure TBreakExpr.EvalNoResult(exec : TdwsExecution);
begin
   exec.Status:=esrBreak;
end;

{ TExitExpr }

procedure TExitExpr.EvalNoResult(exec : TdwsExecution);
begin
   exec.Status:=esrExit;
end;

// ------------------
// ------------------ TExitValueExpr ------------------
// ------------------

// Create
//
constructor TExitValueExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; assignExpr : TNoResultExpr);
begin
   inherited Create(Prog, Pos);
   FAssignExpr:=assignExpr;
end;

// Destroy
//
destructor TExitValueExpr.Destroy;
begin
   FAssignExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TExitValueExpr.EvalNoResult(exec : TdwsExecution);
begin
   FAssignExpr.EvalNoResult(exec);
   exec.Status:=esrExit;
end;

// GetSubExpr
//
function TExitValueExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FAssignExpr;
end;

// GetSubExprCount
//
function TExitValueExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

{ TContinueExpr }

procedure TContinueExpr.EvalNoResult(exec : TdwsExecution);
begin
   exec.Status:=esrContinue;
end;

{ TExceptionExpr }

destructor TExceptionExpr.Destroy;
begin
  FTryExpr.Free;
  FHandlerExpr.Free;
  inherited;
end;

// CreateEDelphiObj
//
function TExceptionExpr.CreateEDelphiObj(exec : TdwsExecution; const ClassName, Message: string): IScriptObj;
var
   info: TProgramInfo;
begin
   info := (exec as TdwsProgramExecution).ProgramInfo;
   Result := IScriptObj(IUnknown(
      info.Vars[SYS_EDELPHI].Method[SYS_TOBJECT_CREATE].Call([ClassName, Message]).Value));
end;

// EnterExceptionBlock
//
function TExceptionExpr.EnterExceptionBlock(exec : TdwsExecution) : Variant;
var
   mainException : Exception;
   err : EScriptError;
   msg : String;
begin
   mainException:=System.ExceptObject as Exception;

   if mainException is EScriptException then begin
      // a raise-statement created an Exception object
      Result:=EScriptException(mainException).Value
   end else if mainException is EScriptError then begin
      msg:=mainException.Message;
      err:=EScriptError(mainException);
      if Length(err.ScriptCallStack)>0 then
         msg:=msg+' in '+(err.ScriptCallStack[High(err.ScriptCallStack)].Expr as TFuncExpr).FuncSym.QualifiedName;
      if EScriptError(mainException).Pos.Defined then
         msg:=msg+EScriptError(mainException).Pos.AsInfo;
      Result:=CreateEDelphiObj(exec, mainException.ClassName, msg);
   end else begin
      // A Delphi exception. Transform it to a EDelphi-dws exception
      Result:=CreateEDelphiObj(exec, mainException.ClassName, mainException.Message);
   end;

   exec.ExceptionObjectStack.Push(Result);
end;

// LeaveExceptionBlock
//
procedure TExceptionExpr.LeaveExceptionBlock(exec : TdwsExecution);
begin
   exec.ExceptionObjectStack.Peek:=Unassigned;
   exec.ExceptionObjectStack.Pop;
end;

// GetSubExpr
//
function TExceptionExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FTryExpr;
      1 : Result:=FHandlerExpr;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TExceptionExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

{ TExceptExpr }

destructor TExceptExpr.Destroy;
begin
  inherited;
  FDoExprs.Clean;
  FElseExpr.Free;
end;

// EvalNoResult
//
procedure TExceptExpr.EvalNoResult(exec : TdwsExecution);
var
   x : Integer;
   obj : Variant;
   objSym : TTypeSymbol;
   doExpr : TExceptDoExpr;
   isCaught : Boolean;
   isReraise : Boolean;
begin
   try
      exec.DoStep(FTryExpr);
      FTryExpr.EvalNoResult(exec);
   except
      if not (System.ExceptObject is Exception) then raise;

      obj:=EnterExceptionBlock(exec);
      try

         isReraise := False;

         // script exceptions
         if FDoExprs.Count > 0 then begin

            isCaught := False;
            objSym := IScriptObj(IUnknown(Obj)).ClassSym;

            for x := 0 to FDoExprs.Count - 1 do begin
               // Find a "on x: Class do ..." statement matching to this exception class
               doExpr := TExceptDoExpr(FDoExprs.List[x]);
               if doExpr.ExceptionVar.Typ.IsCompatible(objSym) then begin
                  exec.Stack.Data[exec.Stack.BasePointer +  doExpr.FExceptionVar.StackAddr] := obj;
                  try
                     exec.DoStep(doExpr);
                     doExpr.EvalNoResult(exec);
                  except
                     on E : EReraise do isReraise := True;
                  end;
                  if isReraise then break;
                  VarClear(exec.Stack.Data[exec.Stack.BasePointer + doExpr.FExceptionVar.StackAddr]);
                  isCaught := True;
                  Break;
               end;
            end;

            if (not isReraise) and (not isCaught) and Assigned(FElseExpr) then begin
               try
                  exec.DoStep(FElseExpr);
                  FElseExpr.EvalNoResult(exec);
               except
                  on E : EReraise do isReraise := True;
               end;
            end;

         end else begin

            try
               exec.DoStep(FHandlerExpr);
               FHandlerExpr.EvalNoResult(exec);
            except
               on E : EReraise do isReraise := True;
            end;

         end;

      finally
         LeaveExceptionBlock(exec);
      end;

      if isReraise then raise;
   end;
   exec.ClearScriptError;
end;

// AddDoExpr
//
procedure TExceptExpr.AddDoExpr(expr : TExceptDoExpr);
begin
   FDoExprs.Add(expr);
end;

// GetSubExpr
//
function TExceptExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i<2 then
      Result:=inherited GetSubExpr(i)
   else if i<2+FDoExprs.Count then
      Result:=TExprBase(FDoExprs.List[i-2])
   else Result:=FElseExpr;
end;

// GetSubExprCount
//
function TExceptExpr.GetSubExprCount : Integer;
begin
   Result:=3+FDoExprs.Count;
end;

// ------------------
// ------------------ TFinallyExpr ------------------
// ------------------

// EvalNoResult
//
procedure TFinallyExpr.EvalNoResult(exec : TdwsExecution);
var
   oldStatus : TExecutionStatusResult;
begin
   try
      exec.DoStep(FTryExpr);
      FTryExpr.EvalNoResult(exec);
   finally
      oldStatus:=exec.Status;
      try
         exec.Status:=esrNone;
         if System.ExceptObject is Exception then begin
            EnterExceptionBlock(exec);
            try
               exec.DoStep(FHandlerExpr);
               FHandlerExpr.EvalNoResult(exec);
            finally
               LeaveExceptionBlock(exec);
            end;
         end else begin
            exec.DoStep(FHandlerExpr);
            FHandlerExpr.EvalNoResult(exec);
         end;
      finally
         exec.Status:=oldStatus;
      end;
   end;
end;

// ------------------
// ------------------ TRaiseExpr ------------------
// ------------------

// Create
//
constructor TRaiseExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; ExceptionExpr: TTypedExpr);
begin
   inherited Create(Prog, Pos);
   FExceptionExpr:=ExceptionExpr;
end;

// Destroy
//
destructor TRaiseExpr.Destroy;
begin
   FExceptionExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TRaiseExpr.EvalNoResult(exec : TdwsExecution);
var
   exceptVal : Variant;
   exceptMessage : String;
begin
   FExceptionExpr.EvalAsVariant(exec, exceptVal);
   exceptMessage:=VarToStr(IScriptObj(IUnknown(exceptVal)).GetData[0]);
   if exceptMessage<>'' then
      raise EScriptException.Create(Format(RTE_UserDefinedException_Msg, [exceptMessage]),
                                    exceptVal, FExceptionExpr.Typ, FScriptPos)
   else raise EScriptException.Create(RTE_UserDefinedException,
                                      exceptVal, FExceptionExpr.Typ, FScriptPos);
end;

// GetSubExpr
//
function TRaiseExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExceptionExpr;
end;

// GetSubExprCount
//
function TRaiseExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TReraiseExpr ------------------
// ------------------

// EvalNoResult
//
procedure TReraiseExpr.EvalNoResult(exec : TdwsExecution);
begin
   raise EReraise.Create('');
end;

// ------------------
// ------------------ TExceptDoExpr ------------------
// ------------------

// Destroy
//
destructor TExceptDoExpr.Destroy;
begin
   FDoBlockExpr.Free;
   FExceptionVar.Free;
   inherited;
end;

// EvalNoResult
//
procedure TExceptDoExpr.EvalNoResult(exec : TdwsExecution);
begin
   DoBlockExpr.EvalNoResult(exec);
end;

// GetSubExpr
//
function TExceptDoExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FDoBlockExpr;
end;

// GetSubExprCount
//
function TExceptDoExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TStringArraySetExpr ------------------
// ------------------

// Create
//
constructor TStringArraySetExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                     StringExpr : TDataExpr; IndexExpr, ValueExpr: TTypedExpr);
begin
   inherited Create(Prog,Pos);
   FStringExpr:=StringExpr;
   FIndexExpr:=IndexExpr;
   FValueExpr:=ValueExpr;
end;

// Destroy
//
destructor TStringArraySetExpr.Destroy;
begin
   FStringExpr.Free;
   FIndexExpr.Free;
   FValueExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TStringArraySetExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   s, buf : String;
begin
   FStringExpr.EvalAsString(exec, s);
   i:=FIndexExpr.EvalAsInteger(exec);
   if i>Length(s) then
      RaiseUpperExceeded(exec, i)
   else if i<1 then
      RaiseLowerExceeded(exec, i);
   FValueExpr.EvalAsString(exec, buf);
   s[i]:=buf[1];
   FStringExpr.AssignValue(exec, s);
end;

// GetSubExpr
//
function TStringArraySetExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FStringExpr;
      1 : Result:=FIndexExpr;
      2 : Result:=FValueExpr;
   else
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TStringArraySetExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// ------------------
// ------------------ TVarStringArraySetExpr ------------------
// ------------------

// EvalNoResult
//
procedure TVarStringArraySetExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   c : Char;
   buf : String;
begin
   i:=FIndexExpr.EvalAsInteger(exec);
   if i<1 then
      RaiseLowerExceeded(exec, i)
   else begin
      FValueExpr.EvalAsString(exec, buf);
      c:=buf[1];
      if not TStrVarExpr(FStringExpr).SetChar(exec, i, c) then
         RaiseUpperExceeded(exec, i);
   end;
end;

// ------------------
// ------------------ TSpecialUnaryBoolExpr ------------------
// ------------------

// IsConstant
//
function TSpecialUnaryBoolExpr.IsConstant : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TDefinedExpr ------------------
// ------------------

// EvalAsBoolean
//
function TDefinedExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   name : String;
begin
   Expr.EvalAsString(exec, name);
   Result:=((exec as TdwsProgramExecution).Prog.ConditionalDefines.IndexOf(name)>=0);
end;

// ------------------
// ------------------ TDeclaredExpr ------------------
// ------------------

// EvalAsBoolean
//
function TDeclaredExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   name : String;
begin
   Expr.EvalAsString(exec, name);
   Result:=(FindSymbol((exec as TdwsProgramExecution).Prog.Table, name)<>nil);
end;

// FindSymbol
//
class function TDeclaredExpr.FindSymbol(symbolTable : TSymbolTable; const name : String) : TSymbol;
var
   p : Integer;
   identifier : String;
begin
   p:=Pos('.', name);
   if p<=0 then
      Result:=symbolTable.FindSymbol(name, cvMagic)
   else begin
      Result:=symbolTable.FindSymbol(Copy(name, 1, p-1), cvMagic);
      if Result=nil then Exit;
      identifier:=Copy(name, p+1, MaxInt);
      if Result.ClassType=TUnitSymbol then
         Result:=FindSymbol(TUnitSymbol(Result).Table, identifier)
      else if Result.InheritsFrom(TClassSymbol) then
         Result:=FindSymbol(TClassSymbol(Result).Members, identifier)
      else if Result.InheritsFrom(TRecordSymbol) then
         Result:=FindSymbol(TRecordSymbol(Result).Members, identifier)
      else Result:=nil;
   end;
end;

// ------------------
// ------------------ TBinaryOperators ------------------
// ------------------

// Create
//
constructor TBinaryOperators.Create(table : TSymbolTable);
var
   typInteger : TTypeSymbol;
   typFloat : TTypeSymbol;
   typBoolean : TTypeSymbol;
   typString : TTypeSymbol;
   typVariant : TTypeSymbol;
   typClassOf : TTypeSymbol;

   procedure RegisterRelOp(aToken : TTokenType; intExpr, floatExpr, strExpr, varExpr : TTypedExprClass);
   begin
      RegisterOperator(aToken,   intExpr,    typInteger, typInteger);
      RegisterOperator(aToken,   floatExpr,  typFloat,   typFloat);
      RegisterOperator(aToken,   floatExpr,  typFloat,   typInteger);
      RegisterOperator(aToken,   floatExpr,  typInteger, typFloat);
      RegisterOperator(aToken,   strExpr,    typString,  typString);
      RegisterOperator(aToken,   strExpr,    typString,  typVariant);
      RegisterOperator(aToken,   strExpr,    typVariant, typString);
      RegisterOperator(aToken,   varExpr,    typVariant, typVariant);
      RegisterOperator(aToken,   varExpr,    typFloat,   typVariant);
      RegisterOperator(aToken,   varExpr,    typInteger, typVariant);
      RegisterOperator(aToken,   varExpr,    typVariant, typInteger);
      RegisterOperator(aToken,   varExpr,    typVariant, typFloat);
   end;

begin
   typInteger:=table.FindSymbol(SYS_INTEGER, cvMagic) as TTypeSymbol;
   typFloat:=table.FindSymbol(SYS_FLOAT, cvMagic) as TTypeSymbol;
   typBoolean:=table.FindSymbol(SYS_BOOLEAN, cvMagic) as TTypeSymbol;
   typString:=table.FindSymbol(SYS_STRING, cvMagic) as TTypeSymbol;
   typVariant:=table.FindSymbol(SYS_VARIANT, cvMagic) as TTypeSymbol;
   typClassOf:=table.FindSymbol(SYS_TCLASS, cvMagic) as TTypeSymbol;

   // computation operators

   RegisterOperator(ttPLUS,   TAddIntExpr,      typInteger,  typInteger);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typString,   typString);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typString,   typVariant);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typVariant,  typString);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typInteger,  typFloat);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typInteger);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typFloat);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typVariant);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typVariant,  typFloat);
   RegisterOperator(ttPLUS,   TAddVariantExpr,  typInteger,  typVariant);
   RegisterOperator(ttPLUS,   TAddVariantExpr,  typVariant,  typInteger);
   RegisterOperator(ttPLUS,   TAddVariantExpr,  typVariant,  typVariant);

   RegisterOperator(ttMINUS,  TSubIntExpr,      typInteger,  typInteger);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typInteger,  typFloat);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typInteger);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typFloat);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typVariant);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typVariant,  typFloat);
   RegisterOperator(ttMINUS,  TSubVariantExpr,  typInteger,  typVariant);
   RegisterOperator(ttMINUS,  TSubVariantExpr,  typVariant,  typInteger);
   RegisterOperator(ttMINUS,  TSubVariantExpr,  typVariant,  typVariant);

   RegisterOperator(ttTIMES,  TMultIntExpr,     typInteger,  typInteger);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typInteger,  typFloat);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typInteger);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typFloat);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typVariant);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typVariant,  typFloat);
   RegisterOperator(ttTIMES,  TMultVariantExpr, typInteger,  typVariant);
   RegisterOperator(ttTIMES,  TMultVariantExpr, typVariant,  typInteger);
   RegisterOperator(ttTIMES,  TMultVariantExpr, typVariant,  typVariant);

   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typVariant);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typVariant);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typVariant);

   RegisterOperator(ttDIV,    TDivExpr,         typInteger,  typInteger);
   RegisterOperator(ttDIV,    TDivExpr,         typInteger,  typVariant);
   RegisterOperator(ttDIV,    TDivExpr,         typVariant,  typInteger);
   RegisterOperator(ttDIV,    TDivExpr,         typVariant,  typVariant);

   RegisterOperator(ttMOD,    TModExpr,         typInteger,  typInteger);
   RegisterOperator(ttMOD,    TModExpr,         typInteger,  typVariant);
   RegisterOperator(ttMOD,    TModExpr,         typVariant,  typInteger);
   RegisterOperator(ttMOD,    TModExpr,         typVariant,  typVariant);

   RegisterOperator(ttOR,     TBoolOrExpr,      typBoolean,  typBoolean);
   RegisterOperator(ttOR,     TBoolOrExpr,      typBoolean,  typVariant);
   RegisterOperator(ttOR,     TBoolOrExpr,      typVariant,  typBoolean);
   RegisterOperator(ttOR,     TIntOrExpr,       typInteger,  typInteger);
   RegisterOperator(ttOR,     TIntOrExpr,       typInteger,  typVariant);
   RegisterOperator(ttOR,     TIntOrExpr,       typVariant,  typInteger);
   RegisterOperator(ttOR,     TIntOrExpr,       typVariant,  typVariant);

   RegisterOperator(ttAND,    TBoolAndExpr,     typBoolean,  typBoolean);
   RegisterOperator(ttAND,    TBoolAndExpr,     typBoolean,  typVariant);
   RegisterOperator(ttAND,    TBoolAndExpr,     typVariant,  typBoolean);
   RegisterOperator(ttAND,    TIntAndExpr,      typInteger,  typInteger);
   RegisterOperator(ttAND,    TIntAndExpr,      typInteger,  typVariant);
   RegisterOperator(ttAND,    TIntAndExpr,      typVariant,  typInteger);
   RegisterOperator(ttAND,    TIntAndExpr,      typVariant,  typVariant);

   RegisterOperator(ttXOR,    TBoolXorExpr,     typBoolean,  typBoolean);
   RegisterOperator(ttXOR,    TBoolXorExpr,     typBoolean,  typVariant);
   RegisterOperator(ttXOR,    TBoolXorExpr,     typVariant,  typBoolean);
   RegisterOperator(ttXOR,    TIntXorExpr,      typInteger,  typInteger);
   RegisterOperator(ttXOR,    TIntXorExpr,      typInteger,  typVariant);
   RegisterOperator(ttXOR,    TIntXorExpr,      typVariant,  typInteger);
   RegisterOperator(ttXOR,    TIntXorExpr,      typVariant,  typVariant);

   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typBoolean,  typBoolean);
   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typVariant,  typBoolean);
   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typBoolean,  typVariant);
   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typVariant,  typVariant);

   RegisterOperator(ttSHL,    TShlExpr,         typInteger,  typInteger);
   RegisterOperator(ttSHL,    TShlExpr,         typInteger,  typVariant);
   RegisterOperator(ttSHL,    TShlExpr,         typVariant,  typInteger);
   RegisterOperator(ttSHL,    TShlExpr,         typVariant,  typVariant);

   RegisterOperator(ttSHR,    TShrExpr,         typInteger,  typInteger);
   RegisterOperator(ttSHR,    TShrExpr,         typInteger,  typVariant);
   RegisterOperator(ttSHR,    TShrExpr,         typVariant,  typInteger);
   RegisterOperator(ttSHR,    TShrExpr,         typVariant,  typVariant);

   // comparison operators

   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typBoolean, typBoolean);
   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typVariant, typBoolean);
   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typBoolean, typVariant);

   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typBoolean, typBoolean);
   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typBoolean, typVariant);
   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typVariant, typBoolean);

   RegisterRelOp(ttEQ,     TRelEqualIntExpr, TRelEqualFloatExpr, TRelEqualStringExpr, TRelEqualVariantExpr);
   RegisterRelOp(ttNOTEQ,  TRelNotEqualIntExpr, TRelNotEqualFloatExpr, TRelNotEqualStringExpr, TRelNotEqualVariantExpr);
   RegisterRelOp(ttLESS,   TRelLessIntExpr, TRelLessFloatExpr, TRelLessStringExpr, TRelLessVariantExpr);
   RegisterRelOp(ttLESSEQ, TRelLessEqualIntExpr, TRelLessEqualFloatExpr, TRelLessEqualStringExpr, TRelLessEqualVariantExpr);
   RegisterRelOp(ttGTR,    TRelGreaterIntExpr, TRelGreaterFloatExpr, TRelGreaterStringExpr, TRelGreaterVariantExpr);
   RegisterRelOp(ttGTREQ,  TRelGreaterEqualIntExpr, TRelGreaterEqualFloatExpr, TRelGreaterEqualStringExpr, TRelGreaterEqualVariantExpr);

   RegisterOperator(ttEQ,     TRelEqualStringExpr,    typClassOf, typClassOf);
   RegisterOperator(ttNOTEQ,  TRelNotEqualStringExpr, typClassOf, typClassOf);

   // combined assignment operator

   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignIntExpr,     typInteger,    typInteger);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typInteger);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typInteger,    typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typVariant,    typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typString,     typString);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typString,     typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typVariant,    typString);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typVariant,    typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typInteger,    typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typVariant,    typInteger);

   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignIntExpr,    typInteger,    typInteger);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typInteger);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typInteger,    typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typVariant,    typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typInteger,    typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typVariant,    typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typVariant,    typInteger);

   RegisterOperator(ttTIMES_ASSIGN, TMultAssignIntExpr,     typInteger,     typInteger);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typInteger);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typInteger,     typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typVariant,     typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typInteger,     typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typVariant,     typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typVariant,     typInteger);

   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typInteger);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typInteger);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typInteger);
end;

// RegisterOperator
//
procedure TBinaryOperators.RegisterOperator(aToken : TTokenType; aExprClass : TProgramExprClass;
                                            aLeftType, aRightType : TTypeSymbol);
var
   n : Integer;
begin
   n:=Length(FItems[aToken]);
   SetLength(FItems[aToken], n+1);
   with FItems[aToken][n] do begin
      ExprClass:=aExprClass;
      LeftType:=aLeftType;
      RighType:=aRightType;
   end;
end;

// ExprClassFor
//
function TBinaryOperators.ExprClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TProgramExprClass;
var
   i : Integer;
begin
   if (aLeftType<>nil) and (aRightType<>nil) then begin
      for i:=0 to High(FItems[aToken]) do with FItems[aToken][i] do begin
         if aLeftType.IsOfType(LeftType) and aRightType.IsOfType(RighType) then
            Exit(ExprClass);
      end;
   end;
   Result:=nil;
end;

// BinaryOperatorClassFor
//
function TBinaryOperators.BinaryOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TBinaryOpExprClass;
var
   expr : TProgramExprClass;
begin
   expr:=ExprClassFor(aToken, aLeftType, aRightType);
   if (expr<>nil) and expr.InheritsFrom(TBinaryOpExpr) then
      Result:=TBinaryOpExprClass(expr)
   else Result:=nil;
end;

// AssignmentOperatorClassFor
//
function TBinaryOperators.AssignmentOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TAssignExprClass;
var
   expr : TProgramExprClass;
begin
   expr:=ExprClassFor(aToken, aLeftType, aRightType);
   if (expr<>nil) and expr.InheritsFrom(TAssignExpr) then
      Result:=TAssignExprClass(expr)
   else Result:=nil;
end;

// RelOperatorClassFor
//
function TBinaryOperators.RelOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : TRelOpExprClass;
var
   expr : TProgramExprClass;
begin
   if (aLeftType=aRightType) and (aLeftType is TEnumerationSymbol) then begin
      aLeftType:=aLeftType.Typ;
      aRightType:=aRightType.Typ;
   end;
   expr:=ExprClassFor(aToken, aLeftType, aRightType);
   if (expr<>nil) and expr.InheritsFrom(dwsRelExprs.TRelOpExpr) then
      Result:=TRelOpExprClass(expr)
   else Result:=nil;
end;

end.
