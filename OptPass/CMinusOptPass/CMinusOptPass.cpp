#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <iostream>
#include <list>

using namespace llvm;

namespace {
    struct CMinusOptPass : public FunctionPass {
        CMinusOptPass() : FunctionPass(ID) { }
        static char ID; 

        virtual const char *getPassName() const {
            return "CMinusOptPass";
        }

        virtual bool runOnFunction(Function &F) {
            while(true) {
                bool entry = true;
                std::list<BasicBlock *> remove_blocks;
            
                for (BasicBlock &B: F) {
                    if(entry) {
                        entry = false;
                        continue;
                    }
                
                    int pred_count = 0;
                
                    for(auto it = pred_begin(&B), et = pred_end(&B); it != et; it++) {
                        BasicBlock *pred = *it;
                        pred_count++;
                    } 
                
                    if(pred_count == 0) {
                        remove_blocks.push_back(&B);
                        
                        for(auto it = succ_begin(&B), et = succ_end(&B); it !=et; it++) {
                            BasicBlock *succ = *it;
                            succ->removePredecessor(&B);
                        } 
                    }
                }
            
                if(remove_blocks.size() == 0)
                    break;
            
                for(BasicBlock *B: remove_blocks) {
                    std::cout << "REMOVING: " << std::endl;
                    for(Instruction &I: *B) {
                        I.dump();
                    }
                    B->dropAllReferences();
                    B->removeFromParent();
                }
                std::cout << std::endl;
            }
            
            return true;
        }
    };
}

char CMinusOptPass::ID = 0;
static RegisterPass<CMinusOptPass> 
    X("cminusopt", "CMinusOptPass", false, false);

