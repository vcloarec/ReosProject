#ifndef REOSWATERSHEDMODULE_H
#define REOSWATERSHEDMODULE_H

#define SIP_NO_FILE

#include "reosmodule.h"
#include "reoswatershedtree.h"

class ReosWatershedDelineating;
class ReosGisEngine;
class ReosMeteorologicModelsCollection;

class REOSCORE_EXPORT ReosWatershedModule : public ReosModule
{
    Q_OBJECT
  public:
    ReosWatershedModule( ReosModule *parent, ReosGisEngine *gisEngine );
    ~ReosWatershedModule();

    //! Returns a pointer to the watershed tree
    ReosWatershedTree *watershedTree() const;
    ReosWatershedDelineating *delineatingModule() const;
    ReosMeteorologicModelsCollection *meteoModelsCollection();

    //! Removes all the watersheds
    void reset();

    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context );
    ReosEncodedElement encode( const ReosEncodeContext &context ) const;

  signals:
    void hasBeenReset();

  private:
    ReosWatershedTree *mWatershedTree = nullptr;
    ReosWatershedDelineating *mDelineatingModule = nullptr;
    ReosMeteorologicModelsCollection *mMeteorologicModelsCollection = nullptr;
};

#endif // REOSWATERSHEDMODULE_H
