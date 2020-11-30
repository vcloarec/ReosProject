#include "reoswatershedmodule.h"

#include "reoswatersheddelineating.h"

ReosWatershedModule::ReosWatershedModule( ReosModule *parent, ReosGisEngine *gisEngine ):
  ReosModule( parent ),
  mWatershedTree( new ReosWatershedTree ),
  mDelineatingModule( new ReosWatershedDelineating( this, mWatershedTree.get(), gisEngine ) )
{

}

ReosWatershedTree *ReosWatershedModule::watershedTree() const
{
  return mWatershedTree.get();
}


ReosWatershedDelineating *ReosWatershedModule::delineatingModule() const
{
  return mDelineatingModule;
}
