#ifndef REOSWATERSHEDMODULE_H
#define REOSWATERSHEDMODULE_H

#include "reosmodule.h"
#include "reoswatershedtree.h"

class ReosWatershedDelineating;
class ReosGisEngine;

class ReosWatershedModule : public ReosModule
{
  public:
    ReosWatershedModule( ReosModule *parent, ReosGisEngine *gisEngine );

    //! Returns a pointer to the watershed tree
    ReosWatershedTree *watershedTree() const;

    ReosWatershedDelineating *delineatingModule() const;

  private:
    std::unique_ptr<ReosWatershedTree> mWatershedTree;
    ReosWatershedDelineating *mDelineatingModule = nullptr;
};

#endif // REOSWATERSHEDMODULE_H
