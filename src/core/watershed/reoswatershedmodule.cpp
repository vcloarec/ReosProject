#include "reoswatershedmodule.h"

#include "reoswatersheddelineating.h"

ReosWatershedModule::ReosWatershedModule( ReosModule *parent, ReosGisEngine *gisEngine ):
  ReosModule( parent ),
  mWatershedTree( new ReosWatershedTree( this ) ),
  mDelineatingModule( new ReosWatershedDelineating( this, mWatershedTree, gisEngine ) )
{

}

ReosWatershedTree *ReosWatershedModule::watershedTree() const
{
  return mWatershedTree;
}


ReosWatershedDelineating *ReosWatershedModule::delineatingModule() const
{
  return mDelineatingModule;
}
