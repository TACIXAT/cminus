#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <iostream>

using namespace llvm;

namespace {
	struct CMinusOptPass : public FunctionPass {
		CMinusOptPass() : FunctionPass(ID) { }
		static char ID; 

		virtual const char *getPassName() const {
			return "CMinusOptPass";
		}

		virtual bool runOnFunction(Function &F) {
			for (BasicBlock &B: F) {
				bool term = false;
				for (Instruction &I: B) {
					I.dump();
					if(term) {
						std::cout << "\tRemoving: " << std::endl;
						I.dump();
						I.removeFromParent();
					} else if(I.isTerminator()) {
						//I.dump();
						std::cout << "\tTerm true..." << std::endl;
						term = true;	
					}
				}
				std::cout << "end block" << std::endl;
			}

			return true;
		}
	};
}

char CMinusOptPass::ID = 0;
static RegisterPass<CMinusOptPass> 
	X("cminusopt", "CMinusOptPass", false, false);

