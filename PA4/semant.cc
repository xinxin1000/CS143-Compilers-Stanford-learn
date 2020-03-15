

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <map>
#include <vector>
#include <queue>
#include <set>
#include "list.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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

ClassTable *classtable;

std::map <Symbol, Class_> m_classes;
//std::map <Symbol, Symbol> class_parent;
std::map <Symbol, std::vector<Symbol> > class_children;

typedef SymbolTable<Symbol, method_class> SymTabMethod;
std::map<Symbol, SymTabMethod> methodtable;  //mapping class to SymbolTable
SymbolTable<Symbol, Symbol> SymTabAttr;  //name --> type_decl



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

  install_basic_classes();

    /* ---step1: check class inheritance
                 m_classes :  map<Symbol, Class_>   ---*/

  // first:  iterate through classes, and build inheritance graph
  bool Main_exist = false;

  for(int i = classes->first(); classes->more(i); i = classes->next(i)){

    Class_ cur_class = classes->nth(i); 
    Symbol cur_class_name = cur_class->get_name();

    // cur_class_name can not be SELF_TYPE
    if(cur_class_name == SELF_TYPE){
      semant_error(cur_class) << "Error: SELF_TYPE redeclared!\n";
      return;
    }

    // cur_class_name can not show up twice
    if(m_classes.count(cur_class_name)){
      semant_error(cur_class) << "Error: class " << cur_class_name << " redeclared!\n" ;
      return;
    }
    
    // cur_class's parent can not be Int, Bool, Str, SELF_TYPE 

    Symbol cur_class_parent = cur_class->get_parent();
    if(cur_class_parent == Int || cur_class_parent == Bool || cur_class_parent == Str || cur_class_parent == SELF_TYPE){
      semant_error(cur_class) << "Error: class " << cur_class_name << " invalid inheritance!\n";
      return;
    }


    if(cur_class_name == Main){
      Main_exist = true;
    }

    m_classes[cur_class_name] = cur_class;
    class_children[cur_class_parent].push_back(cur_class_name);
  }

  //check if there is Main class

  if(!Main_exist){
    semant_error()<< "Class Main is not defined.\n" ;
    return ;
  }


  //then, check acylic
  check_inheritance();
  

}

void ClassTable::check_inheritance(){
  /* ---BFS only starting from Symbol Object is not enough.
        Example: A inherits B, B inherits A.
	actually a cycle starting from A and ends at A is the only condition (otherwise redeclaration)
	We need to start from each class and check its parent all the way to Object---  */
 
  for(std::map<Symbol, Class_>::iterator it = m_classes.begin(); it!=m_classes.end(); ++it){
    
    Symbol cur_class = it->first;
    Symbol parent = it->second->get_parent();
    if(cur_class == Object) continue;

    while(parent != Object && parent != it->first){

      // parent undeclared
      if(m_classes.count(parent) == 0){
	semant_error(m_classes[cur_class])<< "Error: class " << cur_class << "'s parent undeclared!\n";
	return ;
      }

      cur_class = parent;
      parent = m_classes[parent]->get_parent();
    }

    // inheritance cycle found
    if(parent!= Object){
      semant_error(it->second)<< "Error: class " << cur_class << " is involved in an inheritance cycle!\n";
    }
  }

}

std::vector<Symbol> ClassTable:: get_path(Class_ c){
  /* --- find the inheritance path of s
     s <=  p <= ... <= Object  
     note the path is in reverse order---  */

  std::vector<Symbol> path ;
  path.push_back(c->get_name());
  
  while(c->get_parent() != No_class){
    c = m_classes[c->get_parent()];
    path.push_back(c->get_name());
  }

  return path;
    
}


bool ClassTable:: check_valid_type(Symbol s1, Symbol s2, Class_ c2){
  /* --- Check if s2<=s1 --- */
  
  if(s1 ==SELF_TYPE){
    return s2 == SELF_TYPE; 
  }

  if(s2 == SELF_TYPE){
    s2 = c2->get_name();
  }

  std::vector<Symbol> path = get_path(c2);
  for(std::vector<Symbol>::iterator it = path.begin(); it!=path.end(); it++){
    if(*it == s1 ){
     return true;
    }
  }

  return false;
  
}


Symbol ClassTable::findLCA(Class_ c1, Class_ c2){
  /* --- Find least commen ancestor of class c1 and c2.
     first get two paths, then get the shorter distance, finally find LCA ---*/
  std::vector<Symbol> path1 = get_path(c1);
  std::vector<Symbol> path2 = get_path(c2);

  Symbol lca;
  for(std::vector<Symbol>::reverse_iterator it1=path1.rbegin(), it2=path2.rbegin();
      it1!=path1.rend() && it2!=path2.rend(); it1++, it2++){

    if(*it1 == *it2){
      lca = *it1;
    }
    else break;
  }

  return lca;
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
	       filename);

    
    //--- add five basic class into std::map<Symbol, Class_> m_classes ---
    m_classes[Object] = Object_class;
    m_classes[Bool] = Bool_class;
    m_classes[Int] = Int_class;                           
    m_classes[Str] = Str_class;
    m_classes[IO] = IO_class;

    class_children[Object].push_back(IO);  //Bool, Int, Str, can not be inhereited
    
    

}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 


//////////////////////////////////////////////////////////////////
//
//  Here are check_type() functions for each constructor class
//
//////////////////////////////////////////////////////////////////

void method_class:: check_type(Class_ c){
  
  //Step1. check if this method is an overridden
  method_class* mc = NULL;


  std::vector<Symbol> path = classtable->get_path(m_classes[c->get_parent()]);
  for(std::vector<Symbol>::reverse_iterator it = path.rbegin(); it!= path.rend() && !mc; ++it){
    mc = methodtable[*it].probe(name);
  }


  // if overriddes, check if return type, number of formals , formals type matches.
  if(mc){ 
    if(mc->get_typert() != return_type){
      classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
		      <<", override with unmatched return type!\n"; 
    }

    Formals fms = mc->get_formals();
    if(fms->len() != formals->len()){
      classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
		      <<", override with unmatched number of formals!\n"; 
    }
    
    else{
      for(int i=fms->first(), j=formals->first(); fms->more(i), formals->more(j); 
	  i = fms->next(i), j = formals->next(j)){
      
        Formal fm1 = fms->nth(i), fm2 = formals->nth(j);
        if(fm1->get_typedc() != fm2->get_typedc()){
	  classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
	  	        <<", override with unmatched type of formals!\n"; 
	  break;
        }
      }
    }
  }
  else{
    // not an overridden method, check if formals names are different, types are valid
    // no formal name self, no SELF_TYPE, check return type
    
    std::set<Symbol> fm_used;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)){
      Formal fm = formals->nth(i);

      if(fm_used.find(fm->get_name()) != fm_used.end()){
	classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
			<<", duplicated formals!\n";
      }
      fm_used.insert(fm->get_name());


      if(m_classes.find(fm->get_typedc()) == m_classes.end()){
	classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
			<<", invalid type for formal!\n";
      }

      if(fm->get_name() == self){
	classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
			<<", invalid formal self!\n";
      }

      if(fm->get_typedc() == SELF_TYPE){
	classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
			<<", invalid formal type SELF_TYPE!\n";
      }
    }
    
  }
  

  //Step2 maintain SymTabAttr and add attr
  SymTabAttr.enterscope();

  for(int i = formals->first(); formals->more(i); i = formals->next(i)){
    Formal fm = formals->nth(i);
    SymTabAttr.addid(fm->get_name(), new Symbol(fm->get_typedc()));
  }


  //Step3 check type of expr and compare with reture type

  Symbol expr_type = expr->get_type(c);

  if(expr_type!= SELF_TYPE && m_classes.find(return_type)== m_classes.end()){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
				<<", invalid return type!\n";
  }
  else{
     Class_ expr_class = expr_type == SELF_TYPE? c: m_classes[expr_type];
     if(!classtable->check_valid_type(return_type, expr_type, expr_class)){ 
       classtable->semant_error(c) << "Error: class "<< c->get_name()<<" method "<< name 
				   <<", return type and expr type unmatched!\n";
     }

  }

  SymTabAttr.exitscope();

}


void attr_class::check_type(Class_ c){
  
  //check declare type is valid, and compare with init type
  
  if(name == self)
    classtable->semant_error(c) << "Error: class "<< c->get_name()
    				<< ", self can not be an attribute!\n "; 

  if(type_decl!= SELF_TYPE && m_classes.find(type_decl)==m_classes.end()){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<<" attribute "<< name 
				<<", invalid declare type!\n";
  }
  else{
    Symbol init_type = init->get_type(c);
    
    Class_ init_type_class = init_type == SELF_TYPE? c: m_classes[init_type];
    
    if(init_type !=No_type && !classtable->check_valid_type(type_decl, init_type, init_type_class)){
      classtable->semant_error(c) << "Error: class "<< c->get_name()<<" attribute "<< name 
				  <<", declare type and init type unmatched!\n";
    }
  }
}



Symbol branch_class::get_type(Symbol s, Class_ c){

  //s is type for e0,
  //check if type_decl is valid,  check if s<<type_decl,  get type of expr
  if(type_decl!= SELF_TYPE && m_classes.find(type_decl)==m_classes.end()){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				 <<", case branch invalid declare type!\n";
     type_decl = Object;
  }
  
  /*
  if(!classtable->check_valid_type(type_decl,s, m_classes[s])){
    classtable->semant_error(c) << "Error: class "<< c->get_name()
				<<", case branch declare type and case expr type unmatched!\n";
  } */ 
  // notice case expression provides runtime type check on e0, so we don't need to check static type
  
  
  SymTabAttr.enterscope();
  SymTabAttr.addid(name, new Symbol(type_decl==SELF_TYPE? c->get_name(): type_decl));
  
  Symbol expr_type = expr->get_type(c);
  SymTabAttr.exitscope();
  
  return expr_type == SELF_TYPE? c->get_name() : expr_type;  
  //change SELF_TYPE to cur class, prepare to find LCA, assume not all branches are SELF_TYPE

}



Symbol typcase_class::get_type(Class_ c){

  //1. get_type of e0
  Symbol e0_type = expr->get_type(c);
  if(e0_type == SELF_TYPE){
    e0_type = c->get_name();
  }
  
  //2. interate all cases, check declare types are distinct, get case type 
  std::set<Symbol> case_typedc;
  std::vector<Symbol> case_typert;
  

  for(int i = cases->first(); cases->more(i); i = cases->next(i)){
    branch_class* cs = (branch_class*)cases->nth(i);

    Symbol td = cs->get_typedc();
    Symbol tr = cs->get_type(e0_type, c);
    
    if(case_typedc.find(td)!= case_typedc.end()){
      classtable->semant_error(c) << "Error: class "<< c->get_name()<<", case branch declare type duplicated!\n";
    }
    else{
      case_typedc.insert(td);
      case_typert.push_back(tr);
    }
    
  }
  
  type = case_typert[0];
  for(int i=1; i<(int)case_typert.size(); ++i)
  	type = classtable->findLCA(m_classes[type], m_classes[case_typert[i]]); //assume not all branch return SELF_TYPE
  

  return type;
}



Symbol assign_class::get_type(Class_ c){
  
  //get type of expr and compare with type of this variable
  Symbol expr_type = expr->get_type(c);
  Symbol* var_type = SymTabAttr.lookup(name);

  if(!var_type){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<< " variable "<< name << ", undelcared!\n";
    return type = Object;
  }

  Class_ expr_class = expr_type == SELF_TYPE? c: m_classes[expr_type];
   
  if(!classtable->check_valid_type(*var_type, expr_type, expr_class)){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<< " variable "<< name 
				<< ", assign type unmatched!\n";
    return type = Object;
  }

  return type = expr_type;
  
}




Symbol static_dispatch_class:: get_type(Class_ c){

  bool has_error = false;

  //1. check if type_name is valid, get the method
  if(m_classes.find(type_name)== m_classes.end()){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<< ", static dispatch type invalid!\n";
    return type = Object;
  }

  Symbol e0_type = expr->get_type(c); 
  
  Class_ e0_class = e0_type == SELF_TYPE? c: m_classes[e0_type];

  if(!classtable->check_valid_type(type_name, e0_type, e0_class)){
    classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", static dispatch type_name is not ancestor of e0!\n";
    return type = Object;
  }

  //notice that even static dispatch, the method may also not defined in that type_name class.
  std::vector<Symbol> path = classtable->get_path(m_classes[type_name]);

  method_class* md = NULL;
  for(std::vector<Symbol>::iterator it = path.begin(); it!= path.end() && !md; ++it){
    md = methodtable[*it].probe(name);
  }
    
  if(!md){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<< ", static dispatch method can not found!\n";
    return type = Object;
  }
  
  //2. get return type of each actual, and compare with method formal type
  Formals fms = md->get_formals();
  if(fms->len() != actual->len()){
    classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", static dispatch numbers of param unmatched!\n";
    return type = Object;
  }
    
  for(int i = fms->first(); fms->more(i); i = fms->next(i)){
    Symbol ta = actual->nth(i)->get_type(c);
    Symbol tf = fms->nth(i)->get_typedc();

    Class_ ta_class = ta==SELF_TYPE? c: m_classes[ta];
    
    if(!classtable->check_valid_type(tf, ta, ta_class)){
      classtable->semant_error(c) << "Error: class "<< c->get_name()<< ", static dispatch param type unmatched!\n";
      has_error = true;
    }
  }

  //3. return type is method's return type
  if(has_error) return type = Object;


  Symbol rttype = md->get_typert();
  type = rttype == SELF_TYPE? e0_type: rttype;
  return type;
  
  // --- notice the logic for SELF_TYPE here!
  // --- if method returns non-SELF_TYPE, then the dispatch directly returns that;
  // --- if method returns SELF_TYPE, if the dispatch is on self, returns SELF_TYPE;
  // ---                              otherwise, returns e0_type. 

}
   



Symbol dispatch_class::get_type(Class_ c){

  bool has_error = false;

  //1.get return type of expr, check if there is method in that class 
  Symbol e0_type = expr->get_type(c); 
  
  //notice that the method may be defined in parent class.
  if(e0_type == SELF_TYPE){
    e0_type = c->get_name();
  }
  std::vector<Symbol> path = classtable->get_path(m_classes[e0_type]);

  method_class* md = NULL;
  for(std::vector<Symbol>::iterator it = path.begin(); it!= path.end() && !md; ++it){
    md = methodtable[*it].probe(name);
  }

  if(!md){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<< ", dispatch can not find method!\n";
    return type =  Object;
  }

  //2.check actual number and type 
  Formals fms = md->get_formals();
  if(fms->len() != actual->len()){
    classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", dispatch numbers of param unmatched!\n";
    return type = Object;
  }
    
  for(int i = fms->first(); fms->more(i) ; i = fms->next(i)){
    Symbol ta = actual->nth(i)->get_type(c);
    Symbol tf = fms->nth(i)->get_typedc();

    Class_ ta_class = ta==SELF_TYPE? c: m_classes[ta];

    if(!classtable->check_valid_type(tf, ta, ta_class)){
      classtable->semant_error(c) << "Error: class "<< c->get_name()<< ", dispatch param type unmatched!\n";
      has_error = true;
    }
  }
  
  //3.return type
  if(has_error) return type = Object;
  
  
  Symbol rttype = md->get_typert();
  type = rttype == SELF_TYPE? expr->get_type(c): rttype;  
  //to avoid e0_type is SELF_TYPE(self) and has been changed before
  
  return type;
 
}



Symbol cond_class::get_type(Class_ c){
  
  //check pred is Bool, return LCA of then_exp and else_exp
  Symbol pd = pred->get_type(c);
  if(pd != Bool){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<< ", predicate for if is not Bool type!\n";
  }

  Symbol th = then_exp->get_type(c);
  Symbol el = else_exp->get_type(c);
  
  type = classtable->findLCA(th==SELF_TYPE? c: m_classes[th],  el==SELF_TYPE? c:m_classes[el]);

  return type;  //assume then and else don't return SELF_TYPE together
}




Symbol loop_class:: get_type(Class_ c){

  //check pred is Bool, check body type, return Object
  Symbol pd = pred->get_type(c);
  if(pd!= Bool){
    classtable->semant_error(c) << "Error: class "<< c->get_name()<< ", predicate for loop is not Bool type!\n";
  }
  
  body->get_type(c);
  type = Object;
  return type;
}



Symbol block_class:: get_type(Class_ c){
  
  //check type for each expr, and return last one

  for(int i=body->first(); body->more(i); i = body->next(i)){
    type = body->nth(i)->get_type(c);
  }

  return type;
}



Symbol let_class::get_type(Class_ c){
  
  //get type of init and compare with type_decl
  //maintain the SymTabAttr and get return type of body
  if(identifier == self) {
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", identifier can not be self!\n";
  }

  Symbol ini_type = init->get_type(c);
  
  Class_ ini_class = ini_type == SELF_TYPE? c: m_classes[ini_type];

  if(ini_type!= No_type && !classtable->check_valid_type(type_decl, ini_type, ini_class)){
    classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", initial type in let and declare type unmatched!\n";
  }
  
  SymTabAttr.enterscope();
  SymTabAttr.addid(identifier, new Symbol(type_decl));

  type = body->get_type(c);

  SymTabAttr.exitscope();
  return type;

}




Symbol plus_class::get_type(Class_ c){

  Symbol l = e1->get_type(c);
  Symbol r = e2->get_type(c);
  type = Int ;

  if(l != Int || r != Int){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for plus is not Int!\n";
     type = Object;
  }

  return type;
}


Symbol sub_class::get_type(Class_ c){

  Symbol l = e1->get_type(c);
  Symbol r = e2->get_type(c);
  type = Int ;

  if(l != Int || r != Int){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for subtract is not Int!\n";
     type = Object;
  }

  return type;
}


Symbol mul_class::get_type(Class_ c){

  Symbol l = e1->get_type(c);
  Symbol r = e2->get_type(c);
  type = Int ;

  if(l != Int || r != Int){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for multiple is not Int!\n";
     type = Object;
  }

  return type;
}
  

Symbol divide_class::get_type(Class_ c){

  Symbol l = e1->get_type(c);
  Symbol r = e2->get_type(c);
  type = Int ;

  if(l != Int || r != Int){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for divide is not Int!\n";
     type = Object;
  }

  return type;
}

  

Symbol neg_class::get_type(Class_ c){

  Symbol s = e1->get_type(c);

  type = Int ;

  if(s != Int){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for negative is not Int!\n";
     type = Object;
  }

  return type;
}



Symbol lt_class::get_type(Class_ c){

  Symbol l = e1->get_type(c);
  Symbol r = e2->get_type(c);
  type = Bool ;

  if(l != Int || r != Int){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for less than is not Int!\n";
     type = Object;
  }

  return type;
}



Symbol eq_class::get_type(Class_ c){

  Symbol l = e1->get_type(c);
  Symbol r = e2->get_type(c);
  type = Bool;

  if(l == Int || r == Int || l == Bool ||r == Bool || l == Str ||r==Str){
    if(l != r){
      classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", types for equal are not same!\n";
      type = Object;
    }
  }

  return type;
}



Symbol leq_class::get_type(Class_ c){

  Symbol l = e1->get_type(c);
  Symbol r = e2->get_type(c);
  type = Bool ;

  if(l != Int || r != Int){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for less equal than is not Int!\n";
     type = Object;
  }

  return type;
}




Symbol comp_class::get_type(Class_ c){

  Symbol s = e1->get_type(c);

  type =Bool ;

  if(s != Bool){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for complement is not Bool!\n";
     type = Object;
  }

  return type;
}


Symbol int_const_class::get_type(Class_ c){

  type = Int;
  return type;
}



Symbol bool_const_class::get_type(Class_ c){

  type = Bool;
  return type;
}



Symbol string_const_class::get_type(Class_ c){

  type = Str;
  return type;
}



Symbol new__class::get_type(Class_ c){
  
  //check if new type is valid
  type = type_name;   //notice here the return type may be SELF_TYPE

  if(type_name!= SELF_TYPE && m_classes.find(type_name)== m_classes.end()){
    classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", type for new is invalid!\n";
    type = Object;
  }

  return type;
}



Symbol isvoid_class::get_type(Class_ c){
  
  e1->get_type(c);
  type = Bool;  //notice the return type should always be Bool

  return type;
}



Symbol no_expr_class::get_type(Class_ c){

  type = No_type;
  return type;
}



Symbol object_class::get_type(Class_ c){

  if(name == self){
    type = SELF_TYPE;
    return type;
  }

  Symbol* obj_type_ptr  = SymTabAttr.lookup(name);
  if(!obj_type_ptr){
     classtable->semant_error(c) << "Error: class "<< c->get_name()
				<< ", attribute not declared!\n";
     type = Object;
  }
  else type = *obj_type_ptr;

  return type;
  
}






/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    if(classtable->errors()){
      	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }


    /* --- Step2: Do typechecking recursively on the hierarchy graph. ---*/
    
    //////////////////////////////////////////////////////////////////////////
    
    /* --- Build methodtable
       it should be done preliminarily(not during AST traversal,
       because static dispatch may use TYPE which is not an ancestor. ---*/
    
    //typedef SymbolTable<Symbol, method_class> SymTabMethod;
    //std::map<Symbol, SymTabMethod> methodtable;  //mapping class to SymbolTable
    
    
    for(std::map<Symbol, Class_>::iterator it= m_classes.begin(); it!= m_classes.end(); ++it){
      
      Class_ c = (*it).second;
      methodtable[c->get_name()].enterscope();


      Features f = c->get_features();
      for(int j = f->first(); f->more(j); j = f->next(j)){
	
	Feature ft = f->nth(j);
	
	//if this feature is a method and not a duplicated one, add it into methodtable
	if(ft->is_method() && !methodtable[c->get_name()].probe(ft->get_name())  ){
	  
	  methodtable[c->get_name()].addid(ft->get_name(), (method_class*)ft);   
	  //seems ok not create a new method_class, just turn it into required pointer
	}
      }

    }
    
    ////////////////////////////////////////////////////////////////////

    /* --- Start from Object on the graph 
       maintain an SymTabAttr during the traversal --- */
    
    traverse(m_classes[Object]);
   

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


void program_class::traverse(Class_ c){
  /* --- DFS on class hierarchy and do type checking recursively 
     maintain  SymTabAttr  ---*/
  
  if(c->get_name() != Object && c->get_name() != IO) {
  
	  //Step1 check current Class_ c
	  SymTabAttr.enterscope();
	  
	  bool has_main_method = false;  //check if Main class has main method
	
	
	  Features f = c->get_features();
	
	  std::set<int> dup_attr, dup_method; 
	  std::set<Symbol> dup_md_sym;
	
	  //1. iterate features to store attr  preliminarily.
	  // notice that attributes defined later can also be in the init to define the former 
	  for(int i = f->first(); f->more(i); i = f->next(i)){
	
	    Feature ft = f->nth(i);
	    if(!ft->is_method()){
	      
	      if(SymTabAttr.lookup(ft->get_name()) ){
		classtable->semant_error(c) << "Error: class "<< c->get_name()<<" attribute "<< ft->get_name() 
				<<" redefined!\n";
		dup_attr.insert(i);
	      }
	      else{
		SymTabAttr.addid( ft->get_name(), new Symbol(((attr_class*)ft)->get_typedc()));
	      }
	      
	      // maintain the SymTabAttr has no redefined element for class attr.
	    }
	    else{
	      if(dup_md_sym.find(ft->get_name())!=dup_md_sym.end()){
		dup_method.insert(i);
	      }
	      else dup_md_sym.insert(ft->get_name());
	      
	      if(ft->get_name() == main_meth)  has_main_method = true;
	    }
	  }
	  
	  if(c->get_name() == Main && !has_main_method){
	  	classtable->semant_error(c) << "Error: No 'main' method in class Main. \n";
	  }
	  
	  
	
	  //2. iterate features for typechecking for each feature and store attr
	  for(int i = f->first(); f->more(i); i = f->next(i)){
	    
	    Feature ft = f->nth(i);
	    if(ft->is_method()){
	      if(dup_method.find(i) == dup_method.end()){
		ft->check_type(c);
	      }
	    }
	    else{
	      if(dup_attr.find(i) == dup_attr.end()){
		ft->check_type(c);
	      }
	    }
  	  }
  }


  //Step2: check children recursively
  for(std::vector<Symbol>::iterator it = class_children[c->get_name()].begin();
      it!= class_children[c->get_name()].end(); ++it){
     
    traverse(m_classes[*it]);
  }
  
  if( c->get_name() != Object && c->get_name() != IO )
  	SymTabAttr.exitscope();
}
