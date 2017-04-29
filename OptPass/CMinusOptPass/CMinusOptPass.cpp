#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

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
				for (Instruction &I: B) {
					I.dump();	
				}
			}

			return true;
		}
	};
}

char CMinusOptPass::ID = 0;
static RegisterPass<CMinusOptPass> 
	X("cminusopt", "CMinusOptPass", false, false);

