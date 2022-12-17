#include "reoswatershedmodule.h"

#include "reoswatersheddelineating.h"
#include "reosconcentrationtimecalculation.h"
#include "reosmeteorologicmodel.h"
#include "reosrainfallregistery.h"
#include "reostransferfunction.h"

ReosWatershedModule::ReosWatershedModule( ReosModule *parent, ReosGisEngine *gisEngine ):
  ReosModule( parent ),
  mWatershedTree( new ReosWatershedTree( gisEngine, this ) ),
  mDelineatingModule( new ReosWatershedDelineating( this, mWatershedTree, gisEngine ) ),
  mMeteorologicModelsCollection( new ReosMeteorologicModelsCollection( this ) )
{
  ReosTransferFunctionFactories::instantiate( this );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionLinearReservoirFactory );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionGeneralizedRationalMethodFactory );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionSCSUnitHydrographFactory );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionNashUnitHydrographFactory );

  connect( mWatershedTree, &ReosWatershedTree::watershedChanged, this, &ReosModule::dirtied );
  connect( mWatershedTree, &ReosWatershedTree::watershedAdded, this, &ReosModule::dirtied );
  connect( mWatershedTree, &ReosWatershedTree::watershedRemoved, this, &ReosModule::dirtied );

  connect( mMeteorologicModelsCollection, &ReosMeteorologicModelsCollection::changed, this, &ReosModule::dirtied );
}

ReosWatershedModule::~ReosWatershedModule()
{
  if ( ReosConcentrationTimeFormulasRegistery::isInstantiate() )
    delete ReosConcentrationTimeFormulasRegistery::instance();
}

ReosWatershedTree *ReosWatershedModule::watershedTree() const
{
  return mWatershedTree;
}


ReosWatershedDelineating *ReosWatershedModule::delineatingModule() const
{
  return mDelineatingModule;
}

void ReosWatershedModule::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != QStringLiteral( "watershed-module" ) )
    return;

  mWatershedTree->decode( element.getEncodedData( QStringLiteral( "watershed-tree" ) ), context );
  mDelineatingModule->decode( element.getEncodedData( QStringLiteral( "delineating-module" ) ) );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    mMeteorologicModelsCollection->decode( element.getEncodedData( QStringLiteral( "meteo-models-collection" ) ), mWatershedTree, ReosRainfallRegistery::instance() );
  }

  emit hasBeenReset();
}

ReosEncodedElement ReosWatershedModule::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement ret( QStringLiteral( "watershed-module" ) );
  ret.addEncodedData( QStringLiteral( "watershed-tree" ), mWatershedTree->encode( context ) );
  ret.addEncodedData( QStringLiteral( "delineating-module" ), mDelineatingModule->encode() );
  ret.addEncodedData( QStringLiteral( "meteo-models-collection" ), mMeteorologicModelsCollection->encode( mWatershedTree ) );

  return ret;
}

ReosMeteorologicModelsCollection *ReosWatershedModule::meteoModelsCollection()
{
  return mMeteorologicModelsCollection;
}

void ReosWatershedModule::reset()
{
  mWatershedTree->clearWatersheds();
  mDelineatingModule->clear();
  mMeteorologicModelsCollection->reset();
  emit hasBeenReset();
}
