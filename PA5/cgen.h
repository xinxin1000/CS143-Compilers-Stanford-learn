#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

#include <vector>
#include <map>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;



class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:

    List<CgenNode> *nds;
    ostream& str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;



// The following methods emit code for
// constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    void code_class_nameTab();
    void code_class_objTab();
    void code_dispTab();
    void code_protObj();

    void code_class_init();
    void code_class_methods();


// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);

    void get_class_nodes();
    void build_CgenNodes();



public:

    std::vector<CgenNodeP> class_nodes;
    std::map<Symbol, int> class_tags;

    CgenClassTable(Classes, ostream& str);
    void code();
    CgenNodeP root();


    CgenNodeP get_class_node(Symbol c){
        return class_nodes[class_tags[c]];
    }

 
};




class CgenNode : public class__class {

private: 

   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

  
   int class_tag;

   std::vector<CgenNodeP> parents;
   std::vector<attr_class*> attrs;
   std::vector<attr_class*> all_attrs;
   std::map<Symbol, int> attr_ids;

   std::vector<method_class*> meths;
   std::vector<method_class*> all_meths;
   std::map<Symbol, int> meth_ids;
   std::map<Symbol, Symbol> meth_class;


   void code_protObj(ostream&);
   void code_init(ostream&);
   void code_methods(ostream&);


};




class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};



class Environment{
public:

    std::vector<int> scope_lens;
    std::vector<Symbol> var_tab;
    std::vector<Symbol> param_tab;

    CgenNodeP class_node;


    Environment():  class_node(NULL){}

    void EnterScope(){

        scope_lens.push_back(0);
    }

    void ExitScope(){

        for(int i=0; i<scope_lens.back(); ++i){
	    var_tab.pop_back();
	}

	scope_lens.pop_back();
    }

    int lookupAttr(Symbol s){

      std::map<Symbol, int> attr_tab = class_node->attr_ids;
      if(attr_tab.count(s))
 	  return attr_tab[s];
      return -1;
    }

    int lookupVar(Symbol s){

      //vars are in reverse order

        for(int i=0, n=var_tab.size(); i<n; ++i){
  	    if(var_tab[n-1-i] == s)
	        return i;
	}
	return -1;
    }

    int addVar(Symbol s){

        var_tab.push_back(s);
	scope_lens.back()++;
	return var_tab.size()-1;
    }

    int lookupParam(Symbol s){

        for(int i=0, n=param_tab.size(); i<n; ++i){
  	    if(param_tab[n-1-i] == s)  //?
	        return i;
	}
	return -1;
    }

    int addParam(Symbol s){

        param_tab.push_back(s);
	return param_tab.size()-1;
    }

    int addNotype();

};
