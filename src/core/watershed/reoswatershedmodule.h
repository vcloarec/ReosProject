#ifndef REOSWATERSHEDMODULE_H
#define REOSWATERSHEDMODULE_H


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

    //! Adds a watershed from \a delinating polygon and \a outletPoint
    ReosWatershed *addWatershed( const QPolygonF &delineating, const QPointF &ouletPoint );

    //! Removes the \a watershed
    void removeWaterhsed( ReosWatershed *watershed );

    //! Returns a pointer to the watershed tree
    ReosWatershedTree *watershedTree() const SIP_SKIP;
    ReosWatershedDelineating *delineatingModule() const;
    ReosMeteorologicModelsCollection *meteoModelsCollection() SIP_SKIP;

    //! Removes all the watersheds
    void reset() SIP_SKIP;

    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) SIP_SKIP;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const SIP_SKIP;

    static QString staticName() SIP_SKIP {return QStringLiteral( "watershed-module" );}

  signals:
    void hasBeenReset() SIP_SKIP;

  private:
    ReosWatershedTree *mWatershedTree = nullptr;
    ReosWatershedDelineating *mDelineatingModule = nullptr;
    ReosMeteorologicModelsCollection *mMeteorologicModelsCollection = nullptr;
};

#endif // REOSWATERSHEDMODULE_H
