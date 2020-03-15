
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

#include <queue>
#include <algorithm>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;


int labelnum = 0;
CgenClassTableP codegen_classtable = NULL;


//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}



static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);




//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";


  initialize_constants();
  codegen_classtable = new CgenClassTable(classes,os);

  codegen_classtable->code();   //if this is in contructor of CgenClassTable,
  //a lot of code() of other classes can not access CgenClassTable
  codegen_classtable->exitscope();

  os << "\n# end of generated code\n";
}




//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}




///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  
    s  << LABEL                                             // label
       << WORD << stringclasstag << endl                                 // tag
       << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
       << WORD;


 /***** Add dispatch information for class String ******/
    s  << Str << DISPTAB_SUFFIX ;

    s << endl;                                              // dispatch table
    s << WORD;  
    lensym->code_ref(s); 
    s << endl;                                              // string length
    emit_string_constant(s,str);                                // ascii string
    s << ALIGN;                                                 // align to word
}



//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}



//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s); 
    s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
    s << Int << DISPTAB_SUFFIX;
    s << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}




//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
    s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  
    s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
    s << Bool << DISPTAB_SUFFIX;
    s << endl;                                            // dispatch table
    s << WORD << val << endl;                             // value (0 or 1)
}




//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}




//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}



void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}



void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}




//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}




void CgenClassTable::code_class_nameTab(){

  //--- the output in str will be
  //--- -------------------
  //--- class_nameTab:
  //--- .word    str_consti  
  //--- .word    str_constj
  //--- --------------------
  //--- # i is the index for that string in stringtable
  //--- (that is also the reason we need StringEntryP) 

    str << CLASSNAMETAB
        << LABEL;

    for(int i=0, n=class_nodes.size(); i<n; ++i){

        Symbol class_name = class_nodes[i]->name;  //?
	StringEntryP str_entry = stringtable.lookup_string(class_name->get_string());

	str << WORD;
	str_entry->code_ref(str);
	str << endl;
    }
}



void CgenClassTable::code_class_objTab(){

  //--- -------------------
  //--- class_objTab:
  //--- .word    class_nameA_protObj
  //--- .word    class_nameA_init
  //--- .word    class_nameB_protObj
  //--- .word    class_nameB_init
  //--- --------------------

    str << CLASSOBJTAB
	<< LABEL;

    for(int i=0, n=class_nodes.size(); i<n; ++i){

      Symbol class_name = class_nodes[i]->name;
      //StringEntryP str_entry = stringtable.lookup_string(class_name->get_string());

      str << WORD;
      emit_protobj_ref(class_name, str);   //modified
      str << endl;

      str << WORD;
      emit_init_ref(class_name, str);
      str  << endl;
    }
}




void CgenClassTable::code_dispTab(){

  //--- -------------------
  //--- class_nameA_dispTab:
  //--- .word    class_nameP.methodx
  //--- .word    class_nameA.methody
  //--- class_nameB_dispTab
  //--- .word    class_nameP_methodx
  //--- .word    class_nameB_methodz
  //--- --------------------

  for(int i=0, n=class_nodes.size(); i<n; ++i){

      Symbol class_name = class_nodes[i]->name;
      emit_disptable_ref(class_name, str);
      str << LABEL;

      std::vector<method_class*>& all_m = class_nodes[i]->all_meths;
      std::map<Symbol, Symbol>& m_class = class_nodes[i]->meth_class;

      for(int j=0,k=all_m.size(); j<k; ++j){

	Symbol method_name = all_m[j]->name;	

	str << WORD;
	emit_method_ref(m_class[method_name], method_name, str);  
	//notice the class for method is the class in which method is defined most close
	str << endl;
      }

  }

}




void CgenClassTable::code_protObj(){

    for(int i=0, n=class_nodes.size(); i<n; ++i)
        class_nodes[i]->code_protObj(str);
}




void CgenClassTable::code_class_init(){

    //use BFS to init, don't need to init recursively
    std::queue<CgenNodeP> que;
    que.push(class_nodes.front());

    while(que.size()){
	CgenNodeP nd = que.front();
	que.pop();

	nd->code_init(str);

	for(List<CgenNode>* l = nd->get_children(); l; l=l->tl()){
	    que.push(l->hd());
	}
    }
}



void CgenClassTable::code_class_methods(){

    for(int i=0, n=class_nodes.size(); i<n; ++i)
        if(!class_nodes[i]->basic())
            class_nodes[i]->code_methods(str);

}



void CgenNode::code_protObj(ostream& s){

  //--- -------------------
  //--- class_nameA_protObj:
  //--- .word    class_tag_number
  //--- .word    size
  //--- .word    class_nameA_dispTab     # this is the label for classA's methods
  //--- # below are attrs
  //--- .word    int_const_i     # label for Str->val
  //--- .word    0     # for prim_slot or void 
  //--- .word    str_const_i     # label for Str
  //--- --------------------


    s << WORD << "-1" <<endl;
    s << name << PROTOBJ_SUFFIX << LABEL ;   //label
    s << WORD << class_tag << endl;   //class tag
    s << WORD << (DEFAULT_OBJFIELDS + all_attrs.size()) << endl;  //object size
    s << WORD << name << DISPTAB_SUFFIX << endl;


    for(int i=0,n=all_attrs.size(); i<n; ++i){

        if(all_attrs[i]->name == val){  //attr name for Str and Int

	    if(get_name() == Str){  //default value for val(Int) should be 0
	        s << WORD; 
	        inttable.lookup_string("0")->code_ref(s);
		s << endl;
	    }
	    else{
	        s << WORD << "0" <<endl;  //prim_slot type, meaning put 0 on all bits
	    }
	}
	else if(all_attrs[i]->name == str_field){ //attr name for Str

	    s << WORD << "0" <<endl;   //prim_slot type  
	}
	else{  //normal attrs
	  
	    if(all_attrs[i]->type_decl == Bool){
	        s << WORD;
		falsebool.code_ref(s);
		s << endl;
	    }
	    else if(all_attrs[i]->type_decl == Int){
	        s << WORD;
		inttable.lookup_string("0")->code_ref(s);
		s << endl;
	    }
	    else if(all_attrs[i]->type_decl == Str){
  	        s << WORD;
		stringtable.lookup_string("")->code_ref(s);
		s << endl;
	    }
	    else{
	        s << WORD << "0" << endl;  //meaning void
	    }
	}
    }
}




void CgenNode::code_init(ostream& s){

  //--- -------------------
  //--- class_nameA_init:
  //--- # push FP, SELF, RA into stack
  //--- # evaluate each attr and put into SELF
  //--- # put SELF to ACC
  //--- # pop FP, SELF, RA, and return 
  //--- --------------------


    s << name
      << CLASSINIT_SUFFIX 
      << LABEL;

    // push FP, SELF, RA into stack respectively
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    s << endl;

    // FP points to return address in the stack, beginning of possible arguments
    emit_addiu(FP, SP, 4, s);
    s << endl;

    // put $a0 in SELF 
    emit_move(SELF, ACC, s);
    s << endl;


    // now initiate attr newly added in this class
    for(int i=0, n=all_attrs.size(); i<n; ++i){

        int id = attr_ids[all_attrs[i]->name];

	//cout<<all_attrs[i]->name<<endl;

	if(all_attrs[i]->init->isEmpty()){  
	  //if init is no_expr(), deal with basic class

	  if(all_attrs[i]->type_decl == Bool){
	      emit_load_bool(ACC, BoolConst(0), s);
	      emit_store(ACC, 3+id, SELF, s);  //3+id, 3 should be DEFAULT_OBJFIELD
	  }
	  else if(all_attrs[i]->type_decl == Int){
	      emit_load_int(ACC, inttable.lookup_string("0"), s);
	      emit_store(ACC, 3+id, SELF, s);  
	  }
	  else if(all_attrs[i]->type_decl == Str){
	      emit_load_string(ACC, stringtable.lookup_string(""), s);
	      emit_store(ACC, 3+id, SELF, s);  
	  }
	  else{
	      emit_store(ZERO, 3+id, SELF, s);
	  }
	  
	  s << endl;
	}

	else{
	    Environment E;  //create an environment to evaluate expr init
	    E.class_node = this;
	    all_attrs[i]->init->code(s, E);


	    emit_store(ACC, 3+id, SELF, s);
	    if(cgen_Memmgr == 1){
	        emit_addiu(A1, SELF, 4*(3+id), s);
		emit_jal("_GenGC_Assign", s);
	    }
	    s << endl;
	}
    }

    //put SELF to $a0 as result
    emit_move(ACC, SELF, s);
    s << endl;

    //pop RA, SELF, FP
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    s << endl;

    //return 
    emit_return(s);
    s << endl;


}



void CgenNode::code_methods(ostream& s){

  for(std::vector<method_class*>::iterator it =all_meths.begin(); it!=all_meths.end(); ++it) //!
      (*it)->code(s, this);
}




void method_class::code(ostream& s, CgenNodeP class_node){

  //--- -------------------
  //--- class_nameA.methodx:
  //--- # push FP, SELF, RA into stack
  //--- # put $a0 to SELF
  //--- # evaluate expr
  //--- # pop FP, SELF, RA, and return 
  //--- --------------------



    emit_method_ref(class_node->name, name, s);
    s << LABEL;

    // push FP, SELF, RA
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    s << endl;

    // FP points to return address in the stack, beginning of possible arguments
    emit_addiu(FP, SP, 4, s);
    s << endl;

    // put $a0 in SELF 
    emit_move(SELF, ACC, s);
    s << endl;


    //--- evaluate expr and put it into ACC

    Environment E;
    E.class_node = class_node;

    //store parameters in E
    for(int i=formals->first(); formals->more(i); i = formals->next(i)){
        E.addParam(((formal_class*)(formals->nth(i)))->name);
    }

    expr->code(s, E);
    s << endl;


    //pop RA, SELF, FP
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    s << endl;

    //pop out arguments
    emit_addiu(SP, SP, 4*count_arg(), s);   //? when is arg push into stack --- the caller's work

    //return 
    emit_return(s);
    s << endl;

}

  




CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
    
    enterscope();
    if (cgen_debug) cout << "Building CgenClassTable" << endl;

    install_basic_classes();
    install_classes(classes);

    get_class_nodes();

    
    stringclasstag = class_tags[Str] ;
    intclasstag =    class_tags[Int];
    boolclasstag =   class_tags[Bool];

    
    build_inheritance_tree();
    build_CgenNodes();


}




void CgenClassTable::get_class_nodes(){

    for(List<CgenNode>* l = nds; l; l=l->tl()){ //nds is in reverse order

        class_nodes.push_back(l->hd());
    }

    std::reverse(class_nodes.begin(), class_nodes.end());

    for(int i=0, n=class_nodes.size(); i<n; ++i){

        class_tags[class_nodes[i]->name] = i;
	class_nodes[i]->class_tag = i;

    }

}




void CgenClassTable::build_CgenNodes(){

    //--- after building_inheritance_tree,
    //---  we can get parents, all_attrs, all_meths for each CgenNode in a BFS


    std::queue<CgenNodeP> que;
    CgenNodeP classObj = class_nodes.front();
    que.push(classObj);  //first CgenNode should be Object

    //deal with Object methods(Object only has 3 meth)
    Features fts = classObj->features;  

    for(int i=fts->first(); fts->more(i); i = fts->next(i)){
        method_class* ft_meth = (method_class*)fts->nth(i);

	(classObj->meths).push_back(ft_meth); 
	(classObj->all_meths).push_back(ft_meth);
	(classObj->meth_ids)[ft_meth->name] = (classObj->all_meths).size()-1;
	(classObj->meth_class)[ft_meth->name] = classObj->name;
    }


    while(que.size()){

        CgenNodeP nd = que.front();
	que.pop();

	for(List<CgenNode> *chi_list = nd->get_children(); chi_list; 
	    chi_list = chi_list->tl()){

	    CgenNodeP chi = chi_list->hd();
	    que.push(chi);

	    //set parents
	    chi->parents = nd->parents;
	    (chi->parents).push_back(nd);  


	    //set attrs and meths
	    chi->all_attrs = nd->all_attrs;
	    chi->attr_ids = nd->attr_ids;

	    chi->all_meths = nd->all_meths;
	    chi->meth_ids = nd->meth_ids;
	    chi->meth_class = nd->meth_class;

	    Features fts = chi->features;

	    for(int i=fts->first(); fts->more(i); i = fts->next(i)){
	        Feature ft = fts->nth(i);

	        if(!ft->is_meth()){  //attr can not override
		    attr_class* ft_attr = (attr_class*)ft;

		    (chi->attrs).push_back(ft_attr);
		    (chi->all_attrs).push_back(ft_attr);
		    (chi->attr_ids)[ft_attr->name] = (chi->all_attrs).size()-1;
		}

		else{
		    method_class* ft_meth = (method_class*)ft;

		    if((chi ->meth_ids).count(ft_meth ->name)){ //an override method
		        int id = (chi->meth_ids)[ft_meth->name];
			(chi ->all_meths)[id] = ft_meth;
			(chi ->meth_class)[ft_meth->name] = chi->name;
		    }
		    else{ //not an override method
		        (chi->meths).push_back(ft_meth); 
		        (chi->all_meths).push_back(ft_meth);
			(chi->meth_ids)[ft_meth->name] = (chi->all_meths).size()-1;
		    }

		    (chi->meth_class)[ft_meth->name] = chi->name;

		}
	    }
	}

    }
}




void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}



// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}



void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}




//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())  
                                            //use assert to deal with Object's parent No_Class?
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}





void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  if (cgen_debug) cout << "coding class name table" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class object table" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding dispatch table" << endl;
  code_dispTab();

  if (cgen_debug) cout << "coding prototype objects" << endl;
  code_protObj();



  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  if (cgen_debug) cout << "coding class initializers" << endl;
  code_class_init();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();



}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************



int Environment::addNotype(){   
   //because No_class is defined in cgen.cc, write this func here

    EnterScope();
    return addVar(No_type);

}



void binary_op_code(Expression e1, Expression e2, ostream &s, Environment E, bool f){

  //---this is a function in this file
  //---it evaluates e1, e2, put them in T1, T2, get interger value from T1, T2

    e1->code(s, E);
    emit_push(ACC, s);
    E.addNotype();  //?

    e2->code(s, E);
    if(f)
      emit_jal("Object.copy", s);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);   //pop e1 to T1 
    emit_move(T2, ACC, s);   //move e2 to T2

    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, T2, s);

}





void assign_class::code(ostream &s, Environment E) {

    expr->code(s, E);

    int i;

    if((i = E.lookupVar(name))!= -1){  //let variable
        emit_store(ACC, i+1, SP, s);
	if(cgen_Memmgr == 1){
	    emit_addiu(A1, SP, 4*(i+1), s);  //!
	    emit_jal( "_GenGC_Assign", s);  //!
	}
    }

    else if((i = E.lookupParam(name))!= -1){ //parameter of dispatch
        emit_store(ACC, i+3, FP, s);
	if(cgen_Memmgr == 1){
	    emit_addiu(A1, FP, 4*(i+3), s);  //!
	    emit_jal( "_GenGC_Assign", s);
	}
    }

    else if((i = E.lookupAttr(name))!= -1){  //attribute of class
        emit_store(ACC, i+3, SELF, s);
	if(cgen_Memmgr == 1){
	    emit_addiu(A1, SELF, 4*(i+3), s);
	    emit_jal( "_GenGC_Assign", s);
	}
    }

}




void static_dispatch_class::code(ostream &s, Environment E) {
  
  //--- 1. evaluate params and push into stack
  //--- 2. evaluate e0
  //--- 3. check e0 isvoid
  //--- 4. get method label in specified class
  //--- 5. jump to the label


    //evaluate params and push into stack
    for(int i=actual->first(); actual->more(i); i= actual->next(i)){

        Expression ac = actual->nth(i);
	ac->code(s, E);
	emit_push(ACC, s);
	E.addNotype();
    }
   
    //evaluate e0
    expr->code(s, E);


    //check if dispatch on void
    emit_bne(ACC, ZERO, labelnum, s);
    emit_load_address(ACC, "str_const0", s) ;  //"str_const0" ? filename?
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(labelnum++, s);


    //locate dispatch table for the class
    std::string addr = type_name->get_string();
    addr += DISPTAB_SUFFIX;
    emit_load_address(T1, (char*)addr.c_str(), s);
    s << endl;

    //locate the method in the class
    CgenNodeP class_node  = codegen_classtable->get_class_node(type_name);
    int i = class_node->meth_ids[name];   //offset for the method

    emit_load(T1, i, T1, s);
    s << endl;


    //jump to that method
    emit_jalr(T1, s);
    s << endl;

}





void dispatch_class::code(ostream &s, Environment E) {

  //--- 1. evaluate params and push into stack
  //--- 2. evaluate e0
  //--- 3. check e0 isvoid
  //--- 4. get method label in class of e0
  //--- 5. jump to the label

    //evaluate params and push into stack
    for(int i=actual->first(); actual->more(i); i= actual->next(i)){

        Expression ac = actual->nth(i);
	ac->code(s, E);
	emit_push(ACC, s);
	E.addNotype();
    }
   
    //evaluate e0
    expr->code(s, E);

    //cout<<expr->type<<endl;

    //check if dispatch on void
    emit_bne(ACC, ZERO, labelnum, s);
    emit_load_address(ACC, "str_const0", s) ;  //"str_const0" ? filename?
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(labelnum++, s);


    //get class name of expr
    Symbol class_name = E.class_node->name;
    if(expr->get_type() != SELF_TYPE)
        class_name = expr->get_type();


    //locate the class
    emit_load(T1, 2, ACC, s); 
              //?ACC now contain the object of expr, offset 2 is class disptable
    s << endl;


    //locate the method in the class
    CgenNodeP class_node  = (codegen_classtable->get_class_node)(class_name);
    int i = class_node->meth_ids[name];   //offset for the method


    emit_load(T1, i, T1, s);
    s << endl;


    //jump to that method
    emit_jalr(T1, s);
    s << endl;

}




void cond_class::code(ostream &s, Environment E) {
  
  //--- evaluate pred, get interger value of pred
  //--- make branches, false or true, evaluate corresponding expr

    int label_false = labelnum++;
    int label_finish = labelnum++;

    pred->code(s, E);

    emit_fetch_int(T1, ACC, s);

    emit_beq(T1, ZERO, label_false, s);

    then_exp->code(s, E);
    emit_branch(label_finish, s);

    emit_label_def(label_false, s);
    else_exp->code(s, E);

    emit_label_def(label_finish, s);

}




void loop_class::code(ostream &s, Environment E) {

  //--- set 2 labels, evaluate pred compare with ZERO, 
  //--- if not, evaluate body, goto label_start
  //--- if yes, goto label_finish

    int label_start = labelnum++;
    int label_finish = labelnum++;

    emit_label_def(label_start, s);

    pred->code(s, E);
    emit_fetch_int(T1, ACC, s);

    emit_beq(T1, ZERO, label_finish, s);

    body->code(s, E);
    emit_branch(label_start, s);

    emit_label_def(label_finish, s);

    emit_move(ACC, ZERO, s);  //return value is void
}




void typcase_class::code(ostream &s, Environment E) {


    expr->code(s, E);

   //check if case expr is void
    emit_bne(ACC, ZERO, labelnum, s);
    emit_load_address(ACC, "str_const0", s) ;  //"str_const0" : filename
    emit_load_imm(T1, 1, s);
    emit_jal("_case_abort2", s);

    emit_label_def(labelnum++, s);

    emit_load(T1, 0, ACC, s);  //put expr object class_tag  in T1


    int label_begin = labelnum;
    int label_finish = labelnum + count_branch();
    labelnum = label_finish+1;


    //---put each branch into a queue, 
    //---each round, get children of nodes in the queue
    //---instruction will jump to the label first equal T1
    std::vector< std::queue<CgenNodeP> > qs(count_branch());

    //get queues ready
    for(int i=cases->first(); cases->more(i); i=cases->next(i)){
        Symbol case_type = ((branch_class*)(cases->nth(i)))->type_decl;
	qs[i].push(codegen_classtable->get_class_node(case_type));
    }


    bool flag = true;
    while(flag){

      for(int i=0, n=qs.size(); i<n; ++i){
  	  int k = qs[i].size();

	  while(k--){
	    CgenNodeP nd = qs[i].front();
	    qs[i].pop();
	    int case_tag = (codegen_classtable->class_tags)[nd->name];

	    emit_load_imm(T2, case_tag, s);
	    emit_beq(T1, T2, label_begin+i, s);


	    //put nd's children into qs[i]
	    for(List<CgenNode> *l=nd->get_children(); l; l=l->tl())
	        qs[i].push(l->hd());

	  }
      }


      // check if all queues are empty, then ok to end loop
      flag = false;
      for(int i=0, n=qs.size(); i<n&& !flag; ++i)
	if(qs[i].size())
	      flag = true;
    }
    



    /*
    //walk through expr inheritance path,
    //loop through cases each time, until find a same one with expr(or parent)

    CgenNodeP nd = codegen_classtable->get_class_node(expr->get_type());

    while(nd->name != No_class){

        emit_load_imm(T1, nd->class_tag, s);

        for(int i=cases->first(); cases->more(i); i = cases->next(i)){

	    Symbol case_type = ((branch_class*)(cases->nth(i)))->type_decl;
	    int case_tag = (codegen_classtable->class_tags)[case_type];

	    emit_load_imm(T2, case_tag, s);
	    emit_beq(T1, T2, label_begin + i, s);
	    s << endl;
	}

        nd = nd->get_parentnd();
    }
    */



    //if no match
    emit_jal("_case_abort", s);
    emit_branch(label_finish, s);

    

    //set label for each branch
    for(int i = cases->first(); cases->more(i); i= cases->next(i)){
        branch_class* bc = (branch_class*)(cases->nth(i));

	emit_label_def(label_begin + i, s);

	E.EnterScope();    //enter a new scope to evaluate the branch
	E.addVar(bc->name);
	emit_push(ACC, s);

	bc->expr->code(s, E);

	emit_addiu(SP, SP, 4, s);  //pop current let variable
	emit_branch(label_finish, s);
    }

    emit_label_def(label_finish, s);
    s << endl;

}




void block_class::code(ostream &s, Environment E) {

  //--- exprs in block has no relation, simply generate code for each one

    for(int i= body->first(); body->more(i); i = body->next(i)){
        body->nth(i)->code(s, E);
    }
}




void let_class::code(ostream &s, Environment E) {

  //---evaluate init, take care of No_type, keep the init value, evaluate body.

    init->code(s, E);

    //if init is empty, but declare type is basic type, we need to fill in default value
    if(init->isEmpty()){ 
        
        if(type_decl==Int){
	    emit_load_int(ACC, inttable.lookup_string("0"), s);
	}
	else if(type_decl==Str){
	    emit_load_string(ACC, stringtable.lookup_string(""), s);
	}
	else if(type_decl==Bool){
	    emit_load_bool(ACC, BoolConst(0), s);
	}
    }

    emit_push(ACC, s);  
    //notice this is companied with E.addVar, let variables are stored on the stack

    E.EnterScope();
    E.addVar(identifier);

    body->code(s, E);

    emit_addiu(SP, SP, 4, s); //pop out the let variable
}




void plus_class::code(ostream &s, Environment E) {

    binary_op_code(e1, e2, s, E, true);

    emit_add(T3, T1, T2, s);
    emit_store_int(T3, ACC, s);
}




void sub_class::code(ostream &s, Environment E) {

    binary_op_code(e1, e2, s, E, true);

    emit_sub(T3, T1, T2, s);
    emit_store_int(T3, ACC, s);
}




void mul_class::code(ostream &s, Environment E) {

    binary_op_code(e1, e2, s, E, true);

    emit_mul(T3, T1, T2, s);
    emit_store_int(T3, ACC,  s);
}




void divide_class::code(ostream &s, Environment E) {

  //use T1, T2, T3 to deal with integer value, finally store in ACC object

    binary_op_code(e1, e2, s, E, true);
    
    emit_div(T3, T1, T2, s);
    emit_store_int(T3, ACC, s);
}




void neg_class::code(ostream &s, Environment E) {

  //---we first need a copy of original object,
  //---note even it is Int, it is still an object, and we only need to negate 
  //---the interger value in it.

    e1->code(s, E);

    emit_jal("Object.copy", s);  //?
    emit_fetch_int(T1, ACC, s);
    emit_neg(T1, T1, s);
    emit_store_int(T1, ACC, s);
}




void lt_class::code(ostream &s, Environment E) {

    binary_op_code(e1, e2, s, E, false);
    
    emit_load_bool(ACC, BoolConst(1), s);
    emit_blt(T1, T2, labelnum, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum++, s);
}




void eq_class::code(ostream &s, Environment E) {

  //--- There are 2 situation, 
  //--- if e1, e2 have primitive type, use runtime label"equality_test"
  //--- otherwise, compare directly.

    e1->code(s, E);
    emit_push(ACC, s);
    E.addNotype();  //?

    e2->code(s, E);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);   //pop e1 to T1 
    emit_move(T2, ACC, s);   //move e2 to T2


    if(e1->type == Int ||e1->type==Bool ||e1->type==Str) 
      if (e2->type == Int ||e2->type==Bool ||e2->type==Str){  
      //note we have e->type after static type checking phase 

      emit_load_bool(ACC, BoolConst(1), s);
      emit_load_bool(A1, BoolConst(0), s);
      emit_jal("equality_test", s);
      return;
    }

    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, T2, labelnum, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum++, s);
}




void leq_class::code(ostream &s, Environment E) {

  //---use binary_op_code() to evaluete e1, e2, then use branch leq

    binary_op_code(e1, e2, s, E, false);

    emit_load_bool(ACC, BoolConst(1), s);
    emit_bleq(T1, T2, labelnum, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum++, s);

}




void comp_class::code(ostream &s, Environment E) {

  //evaluate e1, get integer value for bool, set ACC to BoolConst 1 or 0 

    e1->code(s, E);

    emit_fetch_int(T1, ACC, s);   //get the int inside bool const(DEFAULTOBJFIEDL = 3)
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, ZERO, labelnum, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum, s);

    ++labelnum;
}




void int_const_class::code(ostream& s, Environment E)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}




void string_const_class::code(ostream& s, Environment E)
{
    emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}




void bool_const_class::code(ostream& s, Environment E)
{
    emit_load_bool(ACC, BoolConst(val), s);
}




void new__class::code(ostream &s, Environment E) {

  //find protObj of this class, get a new copy of it, and initialize it. 
  
    if(type_name == SELF_TYPE){
        
        emit_load_address(T1, CLASSOBJTAB, s);  //
	emit_load(T2, 0, SELF, s);
	emit_sll(T2, T2, 3, s);  //?    
	emit_addu(T1, T1, T2, s);   //get protObj
	emit_push(T1, s);
	emit_load(ACC, 0, T1, s);   //load protObj to ACC
	emit_jal("Object.copy", s);
	emit_load(T1, 1, SP, s);   //pop protObj addr.
	emit_addiu(SP, SP, 4, s);
	emit_load(T1, 1, T1, s);   //get init addr.  ?
	emit_jalr(T1, s);

	return;
    }


    std::string dest = type_name->get_string();
    dest += PROTOBJ_SUFFIX;
    emit_load_address(ACC, ( char*)dest.c_str(), s);
    emit_jal("Object.copy", s);

    dest = type_name->get_string();
    dest += CLASSINIT_SUFFIX;
    emit_jal((char*)dest.c_str(), s);
	
}




void isvoid_class::code(ostream &s, Environment E) {

  //--- evaluate e1, set ACC as BoolConst 0 or 1, use beq to set the branch

    e1->code(s, E);

    emit_move(T1, ACC, s);
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, ZERO, labelnum, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum, s);

    ++labelnum;
    
}



void no_expr_class::code(ostream &s,  Environment E) {

    emit_move(ACC, ZERO, s);
}



void object_class::code(ostream &s, Environment E) {

    //--- There are 4 possibility where the object is from,
    //--- let variable, parameter of dispatch, attribute of class, self

    int i;
    if((i = E.lookupVar(name))!= -1){  //let variable 
        emit_load(ACC, i+1, SP, s);
	if(cgen_Memmgr == 1){
	    emit_addiu(A1, SP, 4*(i+1), s);  //
	    emit_jal( "_GenGC_Assign", s);  //
	}
    }

    else if((i = E.lookupParam(name))!= -1){ //parameter of dispatch
        emit_load(ACC, i+3, FP, s);
	if(cgen_Memmgr == 1){
	    emit_addiu(A1, FP, 4*(i+3), s);  //
	    emit_jal( "_GenGC_Assign", s);
	}
    }

    else if((i = E.lookupAttr(name))!= -1){  //attribute of class
        emit_load(ACC, i+3, SELF, s);
	if(cgen_Memmgr == 1){
	    emit_addiu(A1, SELF, 4*(i+3), s);
	    emit_jal( "_GenGC_Assign", s);
	}
    }

    else{  //self
        emit_move(ACC, SELF, s);
    }
    s << endl;
}


