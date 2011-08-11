start: 
  definitions

global_meta_data: 
  ICE_GLOBAL_METADATA_OPEN string_list ICE_GLOBAL_METADATA_CLOSE

meta_data: 
  ICE_METADATA_OPEN string_list ICE_METADATA_CLOSE

definitions: 
  global_meta_data definitions
| meta_data definition ';' definitions

definition:
  module_def
| class_decl
| class_def
| interface_decl
| interface_def
| exception_decl
| exception_def
| struct_decl
| struct_def
| sequence_def
| dictionary_def
| enum_def
| const_def

module_def:
  ICE_MODULE ICE_IDENTIFIER '{' definitions '}'

exception_id:
  ICE_EXCEPTION ICE_IDENTIFIER
| ICE_EXCEPTION keyword

exception_decl:
  local_qualifier exception_id

exception_def:
  local_qualifier exception_id exception_extends '{' exception_exports '}'

exception_extends:
  ICE_EXTENDS scoped_name
| epsilon

exception_exports:
  meta_data exception_export ';' exception_exports

type_id:
  type ICE_IDENTIFIER

exception_export:
  data_member

struct_id:
  ICE_STRUCT ICE_IDENTIFIER

struct_decl:
  local_qualifier struct_id

struct_def:
  local_qualifier struct_id '{' struct_exports '}'

struct_exports:
  meta_data struct_export ';' struct_exports

struct_export:
  data_member

class_id:
  ICE_CLASS ICE_IDENTIFIER
| ICE_CLASS keyword

class_decl:
  local_qualifier class_id

class_def:
   local_qualifier class_id class_extends implements '{' class_exports '}'

class_extends:
  ICE_EXTENDS scoped_name
| epsilon

class_exports:
  meta_data class_export ';' class_exports

data_member:
  type_id
| type keyword
| type

return_type:
  type
| ICE_VOID

operation_preamble:
  return_type ICE_IDENT_OP
| ICE_NONMUTATING return_type ICE_IDENT_OP
| ICE_IDEMPOTENT return_type ICE_IDENT_OP
| return_type ICE_KEYWORD_OP
| ICE_NONMUTATING return_type ICE_KEYWORD_OP
| ICE_IDEMPOTENT return_type ICE_KEYWORD_OP

operation:
  operation_preamble parameters ')' throws

class_export:
  data_member
| operation

interface_id:
  ICE_INTERFACE ICE_IDENTIFIER

interface_decl:
  local_qualifier interface_id

interface_def:
  local_qualifier interface_id interface_extends '{' interface_exports '}'

interface_list:
  scoped_name ',' interface_list
| scoped_name
| ICE_OBJECT

interface_extends:
  ICE_EXTENDS interface_list

interface_exports:
  meta_data interface_export ';' interface_exports

interface_export:
  operation

exception_list:
  exception ',' exception_list
| exception

exception:
  scoped_name
| keyword

sequence_def:
  local_qualifier ICE_SEQUENCE '<' meta_data type '>' ICE_IDENTIFIER
| local_qualifier ICE_SEQUENCE '<' meta_data type '>' keyword

dictionary_def:
  local_qualifier ICE_DICTIONARY '<' meta_data type ',' meta_data type '>' ICE_IDENTIFIER
| local_qualifier ICE_DICTIONARY '<' meta_data type ',' meta_data type '>' keyword

enum_id:
  ICE_ENUM ICE_IDENTIFIER
| ICE_ENUM keyword

enum_def:
  local_qualifier enum_id '{' enumerator_list '}'

enumerator_list:
  enumerator ',' enumerator_list
| enumerator

enumerator:
  ICE_IDENTIFIER

out_qualifier
  ICE_OUT
| epsilon

parameters:
  epsilon
| out_qualifier meta_data type_id
| parameters ',' out_qualifier meta_data type_id
| out_qualifier meta_data type
| parameters ',' out_qualifier meta_data type

throws:
  ICE_THROWS exception_list

scoped_name:
  ICE_IDENTIFIER
| ICE_SCOPE_DELIMITER ICE_IDENTIFIER
| scoped_name ICE_SCOPE_DELIMITER ICE_IDENTIFIER

type:
  ICE_BYTE | BOOL | SHORT | INT | LONG | FLOAT | DOUBLE | STRING
| OBJECT | OBJECT '*' | LOCAL_OBJECT
| scoped_name
| scoped_name '*'

string_literal:
  ICE_STRING_LITERAL string_literal // Adjacent string literals are concatenated
| ICE_STRING_LITERAL

string_list:
  string_list ',' string_literal
| string_literal

local_qualifier:
  ICE_LOCAL
| epsilon

const_initializer:
  ICE_INTEGER_LITERAL
| ICE_FLOATING_POINT_LITERAL
| scoped_name
| ICE_STRING_LITERAL
| ICE_FALSE
| ICE_TRUE

const_def:
  ICE_CONST meta_data type ICE_IDENTIFIER '=' const_initializer
| ICE_CONST meta_data type '=' const_initializer

keyword:
  ...
