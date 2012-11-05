%% Definition of the Erlang grammar.
Nonterminals
	form
	attribute attr_val typed_attr_val
	macro macro_define
	rule rule_clauses rule_clause rule_body
	function function_clauses function_clause
	clause_args clause_guard guard clause_body
	exprs expr
	expr_100 expr_150 expr_160 expr_200 expr_300 expr_400 expr_500
	expr_600 expr_700 expr_800 expr_900 expr_max
	list tail
	list_comprehension binary_comprehension lc_expr lc_exprs
	tuple
	record_expr record_tuple record_field record_fields
	if_clause if_clauses
	cr_clause cr_clauses
	receive_expr
	fun_expr fun_clause fun_clauses
	try_expr try_catch try_clause try_clauses
	query_expr
	function_call argument_list
	atomic strings
	prefix_op mult_op add_op list_op comp_op
	binary bin_elements bin_element bit_expr opt_bit_size_expr
	bit_size_expr opt_bit_type_list bit_type_list bit_type
	top_type top_type_100 top_types type typed_expr
	type_sig type_sigs type_guard type_guards fun_type fun_type_100 binary_type
	type_spec spec_fun typed_exprs field_types field_type
	bin_base_type bin_unit_type type_200 type_300 type_400 type_500.

Terminals
	char integer float atom string var

	'(' ')' ',' '->' ':-' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.'
	'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
	'andalso' 'orelse' 'query'
	'bnot' 'not'
	'*' '/' 'div' 'rem' 'band' 'and'
	'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
	'++' '--'
	'==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<='
	'<<' '>>'
	'!' '=' '::' '..' '...'
	'?'
	% helper
	'spec'
	'define' 'undef' 'ifdef' 'else' 'endif' 'ifndef'
	'include' 'include_lib'
	dot.

Expect 2.

Rootsymbol form.

form -> attribute dot : '$1'.
form -> function dot  : '$1'.
form -> rule dot      : '$1'.

attribute -> '-' atom attr_val                  : build_attribute('$2', '$3').
attribute -> '-' atom typed_attr_val            : build_typed_attribute('$2','$3').
attribute -> '-' atom '(' typed_attr_val ')'    : build_typed_attribute('$2','$4').
attribute -> '-' 'define' '(' expr ',' expr ')' : build_preprocessor('$2', ['$4', '$6']).
attribute -> '-' 'undef' '(' macro_define ')'   : build_preprocessor('$2', '$4').
attribute -> '-' 'ifdef' '(' macro_define ')'   : build_preprocessor('$2', '$4').
attribute -> '-' 'ifndef' '(' macro_define ')'  : build_preprocessor('$2', '$4').
attribute -> '-' 'else'                         : build_preprocessor('$2', []).
attribute -> '-' 'endif'	                    : build_preprocessor('$2', []).
attribute -> '-' 'include' '(' strings ')'      : build_preprocessor('$2', '$4').
attribute -> '-' 'include_lib' '(' strings ')'  : build_preprocessor('$2', '$4').
attribute -> '-' 'spec' type_spec               : {attribute, ?line('$2'), element(1, '$2'), '$3'}.

attr_val -> expr                   : ['$1'].
attr_val -> expr ',' exprs         : ['$1' | '$3'].
attr_val -> '(' expr ',' exprs ')' : ['$2' | '$4'].

typed_attr_val -> expr ',' '{' typed_exprs '}' : {typed_record, '$1', {tuple, ?line('$3'), '$4'}}.
typed_attr_val -> expr '::' top_type           : {type_def, '$1', '$3'}.

typed_exprs -> typed_expr                 : ['$1'].
typed_exprs -> typed_expr ',' typed_exprs : ['$1' | '$3'].
typed_exprs -> expr ',' typed_exprs       : ['$1' | '$3'].
typed_exprs -> typed_expr ',' exprs       : ['$1' | '$3'].

typed_expr -> expr '::' top_type : {typed,'$1','$3'}.

top_type -> var '::' top_type_100 : {ann_type, ?line('$1'), '$1','$3'}.
top_type -> top_type_100          : '$1'.

top_type_100 -> type_200                  : '$1'.
top_type_100 -> type_200 '|' top_type_100 : lift_unions('$1','$3').

type_200 -> type_300 '..' type_300 : {type, ?line('$1'), range, [skip_paren('$1'), skip_paren('$3')]}.
type_200 -> type_300               : '$1'.

type_300 -> type_300 add_op type_400 : ?mkop2(skip_paren('$1'), '$2', skip_paren('$3')).
type_300 -> type_400                 : '$1'.

type_400 -> type_400 mult_op type_500 : ?mkop2(skip_paren('$1'), '$2', skip_paren('$3')).
type_400 -> type_500                  : '$1'.

type_500 -> prefix_op type : ?mkop1('$1', skip_paren('$2')).
type_500 -> type           : '$1'.

type -> '(' top_type ')'                : {paren_type, ?line('$2'), ['$2']}.
type -> var                             : '$1'.
type -> atom                            : '$1'.
type -> atom '(' ')'                    : build_gen_type('$1', []).
type -> atom '(' top_types ')'          : build_gen_type('$1', '$3').
type -> atom ':' atom '(' ')'           : {remote_type, ?line('$1'), '$1', '$3', []}.
type -> atom ':' atom '(' top_types ')' : {remote_type, ?line('$1'), '$1', '$3', '$5'}.
type -> '[' ']'                         : {type, ?line('$1'), nil, []}.
type -> '[' top_type ']'                : {type, ?line('$1'), list, ['$2']}.
type -> '[' top_type ',' '...' ']'      : {type, ?line('$1'), nonempty_list, ['$2']}.
type -> '{' '}'                         : {type, ?line('$1'), tuple, []}.
type -> '{' top_types '}'               : {type, ?line('$1'), tuple, '$2'}.
type -> '#' atom '{' '}'                : {type, ?line('$1'), record, ['$2']}.
type -> '#' atom '{' field_types '}'    : {type, ?line('$1'), record, ['$2'|'$4']}.
type -> binary_type                     : '$1'.

field_types -> field_type                 : ['$1'].
field_types -> field_type ',' field_types : ['$1' | '$3'].

field_type -> atom '::' top_type : {type, ?line('$1'), field_type, ['$1', '$3']}.

binary_type -> '<<' '>>'                      : {type, ?line('$1'), binary, {0, 0}}.
binary_type -> '<<' bin_base_type '>>'        : {type, ?line('$1'), binary, {'$2', 0}}.
binary_type -> '<<' bin_unit_type '>>'        : {type, ?line('$1'), binary, {0, '$2'}}.
binary_type ->
	'<<' bin_base_type ',' bin_unit_type '>>' : {type, ?line('$1'), binary, {'$2', '$4'}}.

bin_base_type -> var ':' type : build_bin_type(['$1'], '$3').

bin_unit_type -> var ':' var '*' type : build_bin_type(['$1', '$3'], '$5').

type -> integer                    : '$1'.
type -> 'fun' '(' ')'              : {type, ?line('$1'), 'fun', []}.
type -> 'fun' '(' fun_type_100 ')' : '$3'.

fun_type_100 ->
	'(' '...' ')' '->' top_type : {type, ?line('$1'), 'fun', {type, ?line('$1'), any}, '$5'}.
fun_type_100 -> fun_type        : '$1'.

macro_define -> var  : '$1'.
macro_define -> atom : '$1'.

type_spec -> spec_fun type_sigs         : {'$1', '$2'}.
type_spec -> '(' spec_fun type_sigs ')' : {'$2', '$3'}.

type_sigs -> type_sig               : ['$1'].
type_sigs -> type_sig ';' type_sigs : ['$1' | '$3'].

type_sig -> fun_type                    : '$1'.
type_sig -> fun_type 'when' type_guards : {type, ?line('$1'), bounded_fun, ['$1', '$3']}.


fun_type -> '(' ')' '->' top_type           : {type, ?line('$1'), 'fun', {type, ?line('$1'), product, []}, '$4'}.
fun_type -> '(' top_types ')' '->' top_type : {type, ?line('$1'), 'fun', {type, ?line('$1'), product, '$2'},'$5'}.

type_guards -> type_guard                 : ['$1'].
type_guards -> type_guard ',' type_guards : ['$1'|'$3'].

type_guard -> atom '(' top_types ')' : {type, ?line('$1'), constraint, ['$1', '$3']}.
type_guard -> var '::' top_type      : build_def('$1', '$3').

top_types -> top_type               : ['$1'].
top_types -> top_type ',' top_types : ['$1' | '$3'].

spec_fun -> atom                           : '$1'.
spec_fun -> atom ':' atom                  : {remote, ?line('$1'), '$1', '$3'}.

function -> function_clauses : build(function, '$1').

function_clauses -> function_clause      : ['$1'].
function_clauses ->
	function_clause ';' function_clauses : ['$1'|'$3'].

function_clause ->
	atom clause_args clause_guard clause_body : {clause, ?line('$1'), element(3, '$1'), '$2', '$3', '$4'}.

clause_args -> argument_list : element(1, '$1').

argument_list -> '(' ')'      : {[], ?line('$1')}.
argument_list -> '(' exprs ')': {'$2', ?line('$1')}.

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty'     : [].

guard -> exprs           : ['$1'].
guard -> exprs ';' guard : ['$1' | '$3'].

clause_body -> '->' exprs : '$2'.

exprs -> expr           : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

expr -> 'catch' expr : {'catch', ?line('$1'), '$2'}.
expr -> expr_100     : '$1'.

expr_100 -> expr_150 '=' expr_100 : {match,?line('$2'),'$1','$3'}.
expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150              : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160                   : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_200                    : '$1'.

expr_200 -> expr_300 comp_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_200 -> expr_300                  : '$1'.

comp_op -> '=='  : '$1'.
comp_op -> '/='  : '$1'.
comp_op -> '=<'  : '$1'.
comp_op -> '<'   : '$1'.
comp_op -> '>='  : '$1'.
comp_op -> '>'   : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.

expr_300 -> expr_400 list_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_300 -> expr_400                  : '$1'.

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

expr_400 -> expr_400 add_op expr_500 : ?mkop2('$1', '$2', '$3').
expr_400 -> expr_500                 : '$1'.

add_op -> '+'    : '$1'.
add_op -> '-'    : '$1'.
add_op -> 'bor'  : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl'  : '$1'.
add_op -> 'bsr'  : '$1'.
add_op -> 'or'   : '$1'.
add_op -> 'xor'  : '$1'.

expr_500 -> expr_500 mult_op expr_600 : ?mkop2('$1', '$2', '$3').
expr_500 -> expr_600                  : '$1'.

mult_op -> '/'    : '$1'.
mult_op -> '*'    : '$1'.
mult_op -> 'div'  : '$1'.
mult_op -> 'rem'  : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and'  : '$1'.

expr_600 -> prefix_op expr_700 : ?mkop1('$1', '$2').
expr_600 -> expr_700           : '$1'.

prefix_op -> '+'    : '$1'.
prefix_op -> '-'    : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not'  : '$1'.

expr_700 -> function_call : '$1'.
expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

function_call -> expr_800 argument_list : {call, ?line('$1'), '$1', element(1, '$2')}.

%% N.B. Field names are returned as the complete object, even if they are
%% always atoms for the moment, this might change in the future.

record_expr -> '#' atom '.' atom                 : {record_index, ?line('$1'), element(3, '$2'), '$4'}.
record_expr -> '#' atom record_tuple             : {record, ?line('$1'), element(3, '$2'), '$3'}.
record_expr -> expr_max '#' atom '.' atom        : {record_field, ?line('$2'), '$1', element(3, '$3'), '$5'}.
record_expr -> record_expr '#' atom '.' atom     : {record_field, ?line('$2'), '$1', element(3, '$3'), '$5'}.
record_expr -> expr_max '#' atom record_tuple    : {record, ?line('$2'), '$1', element(3, '$3'), '$4'}.
record_expr -> record_expr '#' atom record_tuple : {record, ?line('$2'), '$1', element(3, '$3'), '$4'}.

record_tuple -> '{' '}'               : [].
record_tuple -> '{' record_fields '}' : '$2'.

record_fields -> record_field                   : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '=' expr  : {record_field,?line('$1'),'$1','$3'}.
record_field -> atom '=' expr : {record_field,?line('$1'),'$1','$3'}.

expr_800 -> expr_900 ':' expr_max : {remote, ?line('$2'), '$1', '$3'}.
expr_800 -> expr_900              : '$1'.

expr_900 -> '.' atom              : {record_field, ?line('$1'), {atom, ?line('$1'), ''}, '$2'}.
expr_900 -> expr_900 '.' atom     : {record_field, ?line('$2'), '$1', '$3'}.
expr_900 -> expr_max              : '$1'.

expr_max -> '?' macro                         : {macro_string, ?line('$1'), element(3, '$2')}.
expr_max -> macro                             : '$1'.
expr_max -> var                               : '$1'.
expr_max -> atomic                            : '$1'.
expr_max -> list                              : '$1'.
expr_max -> binary                            : '$1'.
expr_max -> list_comprehension                : '$1'.
expr_max -> binary_comprehension              : '$1'.
expr_max -> tuple                             : '$1'.
expr_max -> '(' expr ')'                      : '$2'.
expr_max -> 'begin' exprs 'end'               : {block, ?line('$1'), '$2'}.
expr_max -> 'if' if_clauses 'end'             : {'if', ?line('$1'), '$2'}.
expr_max -> 'case' expr 'of' cr_clauses 'end' : {'case',?line('$1'),'$2','$4'}.
expr_max -> receive_expr                      : '$1'.
expr_max -> fun_expr                          : '$1'.
expr_max -> try_expr                          : '$1'.
expr_max -> query_expr                        : '$1'.

macro -> '?' var : {macro, ?line('$1'), '$2'}.
macro -> '?' atom : {macro, ?line('$1'), '$2'}.

atomic -> char    : '$1'.
atomic -> integer : '$1'.
atomic -> float   : '$1'.
atomic -> atom    : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings :
	{string, ?line('$1'), element(3, '$1') ++ element(3, '$2')}.

list -> '[' ']'       : {nil, ?line('$1')}.
list -> '[' expr tail : {cons, ?line('$1'), '$2', '$3'}.

tail -> ']'           : {nil, ?line('$1')}.
tail -> '|' expr ']'  : '$2'.
tail -> ',' expr tail : {cons, ?line('$2'), '$2', '$3'}.

binary -> '<<' '>>'              : {bin, ?line('$1'), []}.
binary -> '<<' bin_elements '>>' : {bin, ?line('$1'), '$2'}.

bin_elements -> bin_element                  : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element ->
	bit_expr opt_bit_size_expr opt_bit_type_list : {bin_element, ?line('$1'), '$1', '$2', '$3'}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max           : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty'          : default.

bit_size_expr -> expr_max : '$1'.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty'          : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type                   : ['$1'].

bit_type -> atom                            : element(3, '$1').
bit_type -> atom ':' integer                : {element(3, '$1'), element(3, '$3')}.

list_comprehension -> '[' expr '||' lc_exprs ']' : {lc,?line('$1'),'$2','$4'}.

binary_comprehension -> '<<' binary '||' lc_exprs '>>' : {bc,?line('$1'),'$2','$4'}.

lc_exprs -> lc_expr              : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1' | '$3'].

lc_expr -> expr             : '$1'.
lc_expr -> expr '<-' expr   : {generate, ?line('$2'), '$1', '$3'}.
lc_expr -> binary '<=' expr : {b_generate, ?line('$2'), '$1', '$3'}.

tuple -> '{' '}'       : {tuple, ?line('$1'), []}.
tuple -> '{' exprs '}' : {tuple, ?line('$1'), '$2'}.

if_clauses -> if_clause                : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body : {clause, ?line(hd(hd('$1'))), [], '$1', '$2'}.

cr_clauses -> cr_clause                : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

cr_clause -> expr clause_guard clause_body : {clause, ?line('$1'), ['$1'], '$2', '$3'}.

receive_expr -> 'receive' cr_clauses 'end'               : {'receive', ?line('$1'), '$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' : {'receive', ?line('$1'), [], '$3', '$4'}.
receive_expr ->
	'receive' cr_clauses 'after' expr clause_body 'end'  : {'receive', ?line('$1'), '$2', '$4', '$5'}.

fun_expr -> 'fun' atom '/' integer :
	{'fun', ?line('$1'), element(3, '$2'), element(3, '$4')}.
fun_expr -> 'fun' atom ':' atom '/' integer :
	{'fun', ?line('$1'), element(3, '$2'), element(3, '$4'), element(3,'$6')}.
fun_expr -> 'fun' fun_clauses 'end' :
    Arity = length(element(4, hd('$2'))),
    {'fun', ?line('$1'), {clauses, check_clauses('$2', 'fun', Arity)}}.

fun_clauses -> fun_clause                 : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].

fun_clause -> argument_list clause_guard clause_body :
	{Args,Pos} = '$1',
	{clause, Pos, 'fun', Args, '$2', '$3'}.

try_expr -> 'try' exprs 'of' cr_clauses try_catch :
	{Ccs, As} = '$5',
    {'try', ?line('$1'), '$2', '$4', Ccs, As}.
try_expr -> 'try' exprs try_catch :
	{Ccs, As} = '$3',
    {'try', ?line('$1'), '$2', [], Ccs, As}.

try_catch -> 'catch' try_clauses 'end'               : {'$2',[]}.
try_catch -> 'catch' try_clauses 'after' exprs 'end' : {'$2','$4'}.
try_catch -> 'after' exprs 'end'                     : {[],'$2'}.

try_clauses -> try_clause                 : ['$1'].
try_clauses -> try_clause ';' try_clauses : ['$1' | '$3'].

try_clause -> expr clause_guard clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,[{atom,L,throw},'$1',{var,L,'_'}]}],'$2','$3'}.
try_clause -> atom ':' expr clause_guard clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}],'$4','$5'}.
try_clause -> var ':' expr clause_guard clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}],'$4','$5'}.

query_expr -> 'query' list_comprehension 'end' :
	{'query',?line('$1'),'$2'}.

rule -> rule_clauses : build(rule, '$1').

rule_clauses -> rule_clause                  : ['$1'].
rule_clauses -> rule_clause ';' rule_clauses : ['$1'|'$3'].

rule_clause -> atom clause_args clause_guard rule_body :
	{clause, ?line('$1'), element(3, '$1'), '$2', '$3', '$4'}.

rule_body -> ':-' lc_exprs : '$2'.

Erlang code.

-export([parse_form/1]).

-export([abstract/1]).
-export([abstract/2, package_segments/1]).

-export([inop_prec/1,preop_prec/1,func_prec/0,max_prec/0]).

-export([set_line/2,get_attribute/2,get_attributes/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-export_type([abstract_clause/0, abstract_expr/0, abstract_form/0,
              error_info/0]).

-type abstract_clause() :: term().
-type abstract_expr() :: term().
-type abstract_form() :: term().
-type error_description() :: term().
-type error_info() :: {erl_scan:line(), module(), error_description()}.
-type token() :: {Tag :: atom(), Line :: erl_scan:line()}.

%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

-define(mkop2(L, OpPos, R),
	begin
		{Op,Pos} = OpPos,
		{op,Pos,Op,L,R}
	end).

-define(mkop1(OpPos, A),
	begin
		{Op,Pos} = OpPos,
		{op,Pos,Op,A}
	end).

%% keep track of line info in tokens
-define(line(Tup), element(2, Tup)).

-spec parse_form(Tokens) -> {ok, AbsForm} | {error, ErrorInfo} when
      Tokens :: [token()],
      AbsForm :: abstract_form(),
      ErrorInfo :: error_info().
parse_form([{'-',L1},{atom,L2,Atom}|Tokens]) when
	Atom == define; Atom == undef; Atom == ifdef; Atom == ifndef;
	Atom == else; Atom == endif; Atom == include; Atom == include_lib;
	Atom == spec
->
    parse([{'-',L1},{Atom,L2}|Tokens]);
parse_form(Tokens) ->
    parse(Tokens).

-type attributes() :: 'export' | 'file' | 'import' | 'module'
		    | 'opaque' | 'record' | 'type'.

build_typed_attribute({atom,La,record},
		      {typed_record, {atom,_Ln,RecordName}, RecTuple}) ->
    {attribute,La,record,{RecordName,record_tuple(RecTuple)}};
build_typed_attribute({atom,La,Attr},
                      {type_def, {call,_,{atom,_,TypeName},Args}, Type})
  when Attr =:= 'type' ; Attr =:= 'opaque' ->
    case lists:all(fun({var, _, _}) -> true;
                      (_)           -> false
                   end, Args) of
        true -> {attribute,La,Attr,{TypeName,Type,Args}};
        false -> error_bad_decl(La, Attr)
    end;
build_typed_attribute({atom,La,Attr},_) ->
    case Attr of
        record -> error_bad_decl(La, record);
        type   -> error_bad_decl(La, type);
	opaque -> error_bad_decl(La, opaque);
        _      -> return_error(La, "bad attribute")
    end.

build_preprocessor({Type,La}, Args) ->
    {attribute,La,Type,Args}.

build_def(LHS, Types) ->
    IsSubType = {atom, ?line(LHS), is_subtype},
    {type, ?line(LHS), constraint, [IsSubType, [LHS, Types]]}.

lift_unions(T1, {type, _La, union, List}) ->
    {type, ?line(T1), union, [T1|List]};
lift_unions(T1, T2) ->
    {type, ?line(T1), union, [T1, T2]}.

skip_paren({paren_type,_L,[Type]}) ->
    skip_paren(Type);
skip_paren(Type) ->
    Type.

build_gen_type({atom, La, tuple}, []) ->
    {type, La, tuple, any};
build_gen_type({atom, La, Name}, Args) ->
    {type, La, Name, Args}.

build_bin_type([{var, _, '_'}|Left], Int) ->
    build_bin_type(Left, Int);
build_bin_type([], {integer, _, Int}) ->
	Int;
build_bin_type([{var, La, _}|_], _) ->
    return_error(La, "Bad binary type").

%% build_attribute(AttrName, AttrValue) ->
%%	{attribute,Line,module,Module}
%%	{attribute,Line,export,Exports}
%%	{attribute,Line,import,Imports}
%%	{attribute,Line,record,{Name,Inits}}
%%	{attribute,Line,file,{Name,Line}}
%%	{attribute,Line,Name,Val}

build_attribute({atom, La, module}, Val) ->
	case Val
		of [{atom, _Lm, Module}] ->
			{attribute, La, module, Module}
		; [{atom, _Lm, Module}, ExpList] ->
			{attribute, La, module, Module, var_list(ExpList)}
		; [Name] ->
			case package_segments(Name)
				of error ->
					error_bad_decl(La, module)
				; Module ->
					{attribute, La, module, Module}
			end
		; [Name, ExpList] ->
			case package_segments(Name) of
				error ->
					error_bad_decl(La, module)
				; Module ->
					{attribute, La, module, Module, var_list(ExpList)}
			end
		; _Other ->
			error_bad_decl(La, module)
	end;
build_attribute({atom, La, export}, Val) ->
	case Val
		of [ExpList] ->
			{attribute, La, export, farity_list(ExpList)}
		; _Other ->
			error_bad_decl(La, export)
	end;
build_attribute({atom, La, import}, Val) ->
	case Val
		of [{atom, _Lm, Mod}, ImpList] ->
			{attribute, La, import, {Mod, farity_list(ImpList)}}
		; [Name] ->
			case package_segments(Name)
				of error ->
					error_bad_decl(La, import)
				; Module ->
					{attribute, La, import, Module}
			end
		; [Name, ImpList] ->
			case package_segments(Name)
				of error ->
					error_bad_decl(La, import)
			; Module ->
				{attribute, La, import, {Module, farity_list(ImpList)}}
			end
		; _Other ->
			error_bad_decl(La, import)
	end;
build_attribute({atom, La, record}, Val) ->
	case Val
		of [{atom, _Ln, Record}, RecTuple] ->
			{attribute, La, record, {Record, record_tuple(RecTuple)}}
		; _Other ->
			error_bad_decl(La, record)
	end;
build_attribute({atom, La, file}, Val) ->
	case Val
		of [{string, _Ln, Name}, {integer, _Ll, Line}] ->
			{attribute, La, file, Name, Line};
		_Other ->
			error_bad_decl(La, file)
	end;
build_attribute({atom, La, Attr}, Val) ->
	case Val
		of [Expr0] ->
			Expr = make_attribute_expr(Expr0),
			{attribute, La, Attr, Expr}
		; _Other ->
			return_error(La, "bad attribute")
	end.

var_list({cons,_Lc,{var,_,V},Tail}) ->
    [V|var_list(Tail)];
var_list({nil,_Ln}) -> [];
var_list(Other) ->
    return_error(?line(Other), "bad variable list").

make_attribute_expr({Type, _, _} = Element) when
	Type == char; Type == integer; Type == float; Type == atom; Type == string
->
	Element;
make_attribute_expr({nil, _} = Element) ->
	Element;
make_attribute_expr({bin, Line, Fs}) ->
	{bin, Line, [make_attribute_expr(A) || A <- Fs]};
make_attribute_expr({cons, L, Head, Tail}) ->
	{cons, L, make_attribute_expr(Head), make_attribute_expr(Tail)};
make_attribute_expr({tuple, Line, Args}) ->
	{tuple, Line, [make_attribute_expr(A) || A <- Args]};
make_attribute_expr({record_field, Line, _, _} = A) ->
    case package_segments(A)
		of error ->
			return_error(Line, "bad attribute")
		; _ ->
			A
		end;
make_attribute_expr({op, L, '/', {atom, _, Name}, {integer, _, Arity}}) ->
    {fun_arity, L, Name, Arity};
make_attribute_expr({op, _, Opr, {Par, _, _}} = Element) when
	(Opr == '+') or (Opr == '-'),
		(Par == integer) or (Par == char) or (Par == float)
->
	Element;
make_attribute_expr(Element) ->
	return_error(?line(Element), "bad attribute").

-spec error_bad_decl(integer(), attributes()) -> no_return().

error_bad_decl(L, S) ->
    return_error(L, io_lib:format("bad ~w declaration", [S])).

farity_list({cons,_Lc,{op,_Lo,'/',{atom,_La,A},{integer,_Li,I}},Tail}) ->
    [{A,I}|farity_list(Tail)];
farity_list({nil,_Ln}) -> [];
farity_list(Other) ->
    return_error(?line(Other), "bad function arity").

record_tuple({tuple,_Lt,Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    return_error(?line(Other), "bad record declaration").

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
record_fields([{typed,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    TypeInfo1 =
	case Expr of
	    {match, _, _, _} -> TypeInfo; %% If we have an initializer.
	    {atom, La, _} ->
                case has_undefined(TypeInfo) of
                    false ->
                        TypeInfo2 = maybe_add_paren(TypeInfo),
                        lift_unions(abstract(undefined, La), TypeInfo2);
                    true ->
                        TypeInfo
                end
	end,
    [{typed_record_field,Field,TypeInfo1}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    return_error(?line(Other), "bad record field");
record_fields([]) -> [].

has_undefined({atom,_,undefined}) ->
    true;
has_undefined({ann_type,_,_,T}) ->
    has_undefined(T);
has_undefined({paren_type,_,[T]}) ->
    has_undefined(T);
has_undefined({type,_,union,Ts}) ->
    lists:any(fun has_undefined/1, Ts);
has_undefined(_) ->
    false.

maybe_add_paren({ann_type,L,T}) ->
    {paren_type,L,[{ann_type,L,T}]};
maybe_add_paren(T) ->
    T.

package_segments(Name) ->
    package_segments(Name, [], []).

package_segments({record_field, _, F1, F2}, Fs, As) ->
    package_segments(F1, [F2 | Fs], As);
package_segments({atom, _, A}, [F | Fs], As) ->
    package_segments(F, Fs, [A | As]);
package_segments({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
package_segments(_, _, _) ->
    error.

%% build(Type, Clause) -> {Type,Line,Name,Arity,[Clause]}
build(Type, Clauses) ->
    Name = element(3, hd(Clauses)),
    Arity = length(element(4, hd(Clauses))),
	Line = ?line(hd(Clauses)),
	NewClauses = check_clauses(Clauses, Name, Arity),
    {Type, Line, Name, Arity, NewClauses}.

check_clauses(Cs, Name, Arity) ->
     lists:map(
		fun ({clause, L, N, As, G, B}) when
			N =:= Name, length(As) =:= Arity
		->
			{clause, L, As, G, B}
		; ({clause, L, _N, _As, _G, _B}) ->
			return_error(L, "head mismatch")
		end, Cs).

-spec abstract(Data) -> AbsTerm when
      Data :: term(),
      AbsTerm :: abstract_expr().
abstract(T) when is_integer(T) -> {integer,0,T};
abstract(T) when is_float(T) -> {float,0,T};
abstract(T) when is_atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when is_bitstring(B) ->
    {bin, 0, [abstract_byte(Byte, 0) || Byte <- bitstring_to_list(B)]};
abstract([C|T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

abstract_byte(Byte, Line) when is_integer(Byte) ->
    {bin_element, Line, {integer, Line, Byte}, default, default};
abstract_byte(Bits, Line) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, Line, {integer, Line, Val}, {integer, Line, Sz}, default}.

%%% abstract/2 keeps the line number
abstract(T, Line) when is_integer(T) -> {integer,Line,T};
abstract(T, Line) when is_float(T) -> {float,Line,T};
abstract(T, Line) when is_atom(T) -> {atom,Line,T};
abstract([], Line) -> {nil,Line};
abstract(B, Line) when is_bitstring(B) ->
    {bin, Line, [abstract_byte(Byte, Line) || Byte <- bitstring_to_list(B)]};
abstract([C|T], Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C], Line);
abstract([H|T], Line) ->
    {cons,Line,abstract(H, Line),abstract(T, Line)};
abstract(Tuple, Line) when is_tuple(Tuple) ->
    {tuple,Line,abstract_list(tuple_to_list(Tuple), Line)}.

abstract_string([C|T], String, Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String], Line);
abstract_string([], String, Line) ->
    {string, Line, lists:reverse(String)};
abstract_string(T, String, Line) ->
    not_string(String, abstract(T, Line), Line).

not_string([C|T], Result, Line) ->
    not_string(T, {cons, Line, {integer, Line, C}, Result}, Line);
not_string([], Result, _Line) ->
    Result.

abstract_list([H|T], Line) ->
    [abstract(H, Line)|abstract_list(T, Line)];
abstract_list([], _Line) ->
    [].

%% Give the relative precedences of operators.

inop_prec('=') -> {150,100,100};
inop_prec('!') -> {150,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {200,160,160};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {400,300,300};
inop_prec('--') -> {400,300,300};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.

-type pre_op() :: 'catch' | '+' | '-' | 'bnot' | 'not' | '#'.

-spec preop_prec(pre_op()) -> {0 | 600 | 700, 100 | 700 | 800}.

preop_prec('catch') -> {0,100};
preop_prec('+') -> {600,700};
preop_prec('-') -> {600,700};
preop_prec('bnot') -> {600,700};
preop_prec('not') -> {600,700};
preop_prec('#') -> {700,800}.

-spec func_prec() -> {800,700}.
func_prec() -> {800,700}.

-spec max_prec() -> 1000.
max_prec() -> 1000.

%%% [Experimental]. The parser just copies the attributes of the
%%% scanner tokens to the abstract format. This design decision has
%%% been hidden to some extent: use set_line() and get_attribute() to
%%% access the second element of (almost all) of the abstract format
%%% tuples. A typical use is to negate line numbers to prevent the
%%% compiler from emitting warnings and errors. The second element can
%%% (of course) be set to any value, but then these functions no
%%% longer apply. To get all present attributes as a property list
%%% get_attributes() should be used.

set_line(L, F) ->
    erl_scan:set_attribute(line, L, F).

get_attribute(L, Name) ->
    erl_scan:attributes_info(L, Name).

get_attributes(L) ->
    erl_scan:attributes_info(L).
