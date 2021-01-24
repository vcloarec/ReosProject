#ifndef REOSWATERSHEDMODULE_H
#define REOSWATERSHEDMODULE_H

#include "reosmodule.h"
#include "reoswatershedtree.h"

class ReosWatershedDelineating;
class ReosGisEngine;

class REOSCORE_EXPORT ReosWatershedModule : public ReosModule
{
    Q_OBJECT
  public:
    ReosWatershedModule( ReosModule *parent, ReosGisEngine *gisEngine );

    //! Returns a pointer to the watershed tree
    ReosWatershedTree *watershedTree() const;
    ReosWatershedDelineating *delineatingModule() const;

    void decode( const ReosEncodedElement &element );
    ReosEncodedElement encode() const;

  signals:
    void hasBeenReset();

  private:
    ReosWatershedTree *mWatershedTree = nullptr;
    ReosWatershedDelineating *mDelineatingModule = nullptr;
};

#endif // REOSWATERSHEDMODULE_H
