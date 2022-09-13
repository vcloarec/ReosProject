/***************************************************************************
  reoshydrographsource.cpp - ReosHydrographSource

 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshydrographsource.h"
#include "reoshydrograph.h"
#include "reoscalculationcontext.h"
#include "reoshydrographrouting.h"
#include "reosstyleregistery.h"
#include "reosgisengine.h"
#include "reoshydraulicscheme.h"

ReosHydrographNode::ReosHydrographNode( ReosHydraulicNetwork *parent )
  : ReosHydraulicNode( parent )
{}

ReosHydrographNode::ReosHydrographNode( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydraulicNode( encodedElement, parent )
{}

ReosHydrographSource::ReosHydrographSource( ReosHydraulicNetwork *parent ) : ReosHydrographNode( parent )
{
  mUseForceOutputTimeStep = new ReosParameterBoolean( tr( "Force output time step" ), this );
  mUseForceOutputTimeStep->setValue( false );
  mForceOutputTimeStep = new ReosParameterDuration( tr( "Output time step" ), this ) ;
  mForceOutputTimeStep->setValue( ReosDuration( 5, ReosDuration::minute ) );
}

ReosHydrographSource::ReosHydrographSource( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydrographNode( encodedElement, parent )
{
  mUseForceOutputTimeStep = ReosParameterBoolean::decode(
                              encodedElement.getEncodedData( QStringLiteral( "use-force-output-time-step" ) ), false, tr( "Force output time step" ), this );
  if ( !mUseForceOutputTimeStep->isValid() )
    mUseForceOutputTimeStep->setValue( false );

  mForceOutputTimeStep = ReosParameterDuration::decode(
                           encodedElement.getEncodedData( QStringLiteral( "output-time-step" ) ), false, tr( "Output time step" ), this );
  if ( !mForceOutputTimeStep->isValid() )
    mForceOutputTimeStep->setValue( ReosDuration( 5, ReosDuration::minute ) );
}

void ReosHydrographSource::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext & ) const
{
  element.addEncodedData( QStringLiteral( "use-force-output-time-step" ), mUseForceOutputTimeStep->encode() );
  element.addEncodedData( QStringLiteral( "output-time-step" ), mForceOutputTimeStep->encode() );
}

ReosParameterDuration *ReosHydrographSource::forceOutputTimeStep() const
{
  return mForceOutputTimeStep;
}

ReosParameterBoolean *ReosHydrographSource::useForceOutputTimeStep() const
{
  return mUseForceOutputTimeStep;
}

ReosHydrographRoutingLink *ReosHydrographSource::outputHydrographTransfer() const
{
  if ( mLinksBySide1.isEmpty() )
    return nullptr;
  return qobject_cast<ReosHydrographRoutingLink *>( mLinksBySide1.at( 0 ) );
}

ReosHydrographSourceFixed::ReosHydrographSourceFixed( ReosHydraulicNetwork *parent )
  : ReosHydrographSource( parent )
{}

ReosHydrographSourceFixed::ReosHydrographSourceFixed( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydrographSource( encodedElement, parent )
{}

ReosHydrograph *ReosHydrographSourceFixed::outputHydrograph() const
{
  return mHydrograph;
}

void ReosHydrographSourceFixed::setHydrograph( ReosHydrograph *hydrograph )
{
  mHydrograph = hydrograph;
  mHydrograph->setParent( this );
}

bool ReosHydrographSourceFixed::updateCalculationContextFromDownstream( const ReosCalculationContext &, ReosHydrographRoutingLink * )
{
  if ( isObsolete() )
    QMetaObject::invokeMethod( this, [this] {calculationUpdated();}, Qt::QueuedConnection );
  return false;
}

void ReosHydrographSourceFixed::updateCalculationContextFromUpstream( const ReosCalculationContext &, ReosHydrographRoutingLink *, bool )
{
  if ( isObsolete() )
    QMetaObject::invokeMethod( this, [this] {calculationUpdated();}, Qt::QueuedConnection );
}

void ReosHydrographSourceFixed::updateCalculationContext( const ReosCalculationContext &context )
{
  updateCalculationContextFromUpstream( context, nullptr, false );
}


ReosHydrographJunction::ReosHydrographJunction( const QPointF &position, ReosHydraulicNetwork *parent )
  : ReosHydrographSource( parent )
  , mOutputHydrograph( new ReosHydrograph( this ) ),
    mPosition( position )
{
  mOutputHydrograph->setColor( ReosStyleRegistery::instance()->curveColor() );
  mHydrographsStore = new ReosHydrographsStore( this );
  connect( mHydrographsStore, &ReosHydrographsStore::hydrographChanged, this, &ReosHydraulicNetworkElement::dirtied );
  connect( mHydrographsStore, &ReosHydrographsStore::dataChanged, this, &ReosHydraulicNetworkElement::dirtied );
  init();
}

ReosHydrographJunction::ReosHydrographJunction( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydrographSource( encodedElement, parent )
  , mOutputHydrograph( new ReosHydrograph( this ) )
{
  if ( encodedElement.hasEncodedData( QStringLiteral( "position" ) ) )
  {
    QPointF pos;
    encodedElement.getData( QStringLiteral( "position" ), pos );
    mPosition = ReosSpatialPosition( pos, parent->gisEngine()->crs() );
  }
  else
  {
    mPosition = ReosSpatialPosition::decode( encodedElement.getEncodedData( QStringLiteral( "spatial-position" ) ) );
  }

  if ( mPosition.crs().isEmpty() )
    mPosition = ReosSpatialPosition( mPosition.position(), parent->gisEngine()->crs() );

  QColor outputColor;
  encodedElement.getData( QStringLiteral( "output-color" ), outputColor );
  mOutputHydrograph->setColor( outputColor );
  mHydrographsStore = new ReosHydrographsStore( this );
  mHydrographsStore->decode( encodedElement.getEncodedData( QStringLiteral( "gauged-hydrographs" ) ) );
  connect( mHydrographsStore, &ReosHydrographsStore::hydrographChanged, this, &ReosHydraulicNetworkElement::dirtied );
  connect( mHydrographsStore, &ReosHydrographsStore::dataChanged, this, &ReosHydraulicNetworkElement::dirtied );

  // before v 2.3, this data are stored in the element encoded data, so try to retrieve them here
  int origin = 0;
  if ( encodedElement.hasEncodedData( QStringLiteral( "hydrograph-origin" ) ) )
  {
    encodedElement.getData( QStringLiteral( "hydrograph-origin" ), origin );
    mInternalHydrographOrigin = static_cast<ReosHydrographNodeWatershed::InternalHydrographOrigin>( origin );
  }
  if ( encodedElement.hasEncodedData( QStringLiteral( "gauged-hydrograph-index" ) ) )
    encodedElement.getData( QStringLiteral( "gauged-hydrograph-index" ), mGaugedHydrographIndex );

  init();
}


void ReosHydrographJunction::init()
{
  mOutputHydrograph->setName( outputPrefixName() + QStringLiteral( " %1" ).arg( elementName()->value() ) );
  connect( elementName(), &ReosParameterString::valueChanged, mOutputHydrograph, [this]
  {
    mOutputHydrograph->setName( outputPrefixName() + QStringLiteral( " %1" ).arg( elementName()->value() ) );
  } );

  connect( this, &ReosHydraulicNode::dataChanged, this, [this]
  {
    mNeedCalculation = true;
    mOutputHydrograph->setHydrographObsolete();
  } );

  connect( mUseForceOutputTimeStep, &ReosParameter::valueChanged, this, &ReosHydrographJunction::onTimeStepChange );
  connect( mForceOutputTimeStep, &ReosParameter::valueChanged, this, &ReosHydrographJunction::onTimeStepChange );

  if ( mHydrographsStore )
  {
    connect( mHydrographsStore, &ReosHydrographsStore::hydrographRemoved, this, [this]( int hydrographIndex )
    {
      if ( mGaugedHydrographIndex == hydrographIndex )
      {
        mGaugedHydrographIndex = -1;
        updateInternalHydrograph();
      }
    } );
  }
}

bool ReosHydrographJunction::updateInternalHydrograph()
{
  ReosHydrograph *newHydrograph = nullptr;

  switch ( mInternalHydrographOrigin )
  {
    case ReosHydrographJunction::None:
      newHydrograph = nullptr;
      break;
    case ReosHydrographJunction::GaugedHydrograph:
      newHydrograph = mHydrographsStore->hydrograph( mGaugedHydrographIndex );
      break;
  }

  return setCurrentInternalHydrograph( newHydrograph );
}

ReosHydrograph *ReosHydrographJunction::outputHydrograph() const
{
  return mOutputHydrograph;
}

QPointF ReosHydrographJunction::position( const QString &destinationCrs ) const
{
  return mNetwork->gisEngine()->transformToCoordinates( mPosition, destinationCrs );
}

void ReosHydrographJunction::setPosition( const ReosSpatialPosition &pos )
{
  mPosition = pos;
  positionChanged();
}

bool ReosHydrographJunction::calculationInProgress() const {return mCalculationIsInProgress;}

int ReosHydrographJunction::calculationMaxProgression() const
{
  if ( !mCalculationIsInProgress )
    return 100;

  if ( !mSumCalculation )
    return 0;

  return mSumCalculation->maxProgression();
}

int ReosHydrographJunction::calculationProgression() const
{
  if ( !mCalculationIsInProgress )
    return 100;

  if ( !mSumCalculation )
    return 0;

  return mSumCalculation->currentProgression();
}

void ReosHydrographJunction::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  element.addEncodedData( QStringLiteral( "spatial-position" ), mPosition.encode() );
  element.addData( QStringLiteral( "output-color" ), mOutputHydrograph->color() );

  if ( mHydrographsStore->parent() == this )
  {
    element.addEncodedData( QStringLiteral( "gauged-hydrographs" ), mHydrographsStore->encode() );
  }

  ReosHydrographSource::encodeData( element, context );
}

bool ReosHydrographJunction::updateInternalHydrographCalculationContext( const ReosCalculationContext & )
{
  return updateInternalHydrograph();
}


ReosHydrographJunction *ReosHydrographJunction::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydrographJunction::staticType() )
    return nullptr;

  std::unique_ptr<ReosHydrographJunction> ret( new ReosHydrographJunction( encodedElement, context.network() ) );

  return ret.release();
}

void ReosHydrographJunction::updateCalculationContext( const ReosCalculationContext &context )
{
  updateCalculationContextFromUpstream( context, nullptr, false ); //this lead to update all routing linked to this junction and also this junction
}

void ReosHydrographJunction::updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydrographRoutingLink *upstreamRouting, bool upstreamWillChange )
{
  QList<ReosHydrographRoutingLink *> upstreamLinks = ReosHydraulicNetworkUtils::upstreamLinkOfType<ReosHydrographRoutingLink>( this );

  upstreamLinks.removeOne( upstreamRouting );

  if ( upstreamWillChange )
    mWaitingForUpstreamLinksUpdated.insert( upstreamRouting->id() );

  for ( ReosHydrographRoutingLink *routing : std::as_const( upstreamLinks ) )
  {
    if ( routing )
    {
      bool routingNeedToBeUpdated = routing->updateCalculationContextFromDownstream( context );
      if ( routingNeedToBeUpdated )
        mWaitingForUpstreamLinksUpdated.insert( routing->id() );
      mNeedCalculation |= routingNeedToBeUpdated;
    }
  }

  bool internalHydrographWillChange = updateInternalHydrographCalculationContext( context );

  mNeedCalculation |= internalHydrographWillChange || upstreamWillChange || isObsolete();

  if ( upstreamWillChange && mSumCalculation )
  {
    mSumCalculation->stop( true );
    mSumCalculation = nullptr;
  }

  mCalculationIsInProgress = mNeedCalculation;

  QList<ReosHydrographRoutingLink *> downstreamLinks = ReosHydraulicNetworkUtils::downstreamLinkOfType<ReosHydrographRoutingLink>( this );
  Q_ASSERT( downstreamLinks.count() < 2 );
  if ( !downstreamLinks.isEmpty() )
    downstreamLinks.at( 0 )->updateCalculationContextFromUpstream( context, mNeedCalculation );

  if ( mNeedCalculation )
  {
    calculateIfAllReady();
  }

}


bool ReosHydrographJunction::updateCalculationContextFromDownstream( const ReosCalculationContext &context, ReosHydrographRoutingLink * )
{
  const QList<ReosHydrographRoutingLink *> upstreamLinks = ReosHydraulicNetworkUtils::upstreamLinkOfType<ReosHydrographRoutingLink>( this );
  for ( ReosHydrographRoutingLink *routing : upstreamLinks )
  {
    if ( routing )
    {
      bool routingNeedToBeUpdated = routing->updateCalculationContextFromDownstream( context );
      mNeedCalculation |= routingNeedToBeUpdated;
      if ( routingNeedToBeUpdated )
        mWaitingForUpstreamLinksUpdated.insert( routing->id() );
    }
  }

  mNeedCalculation |= updateInternalHydrographCalculationContext( context ) || isObsolete();

  if ( mNeedCalculation )
  {
    calculateIfAllReady();
  }

  return mNeedCalculation;
}

ReosHydrographRoutingLink *ReosHydrographJunction::downstreamRouting() const
{
  QList<ReosHydrographRoutingLink *> downstreamLinks = ReosHydraulicNetworkUtils::downstreamLinkOfType<ReosHydrographRoutingLink>( this );
  Q_ASSERT( downstreamLinks.count() < 2 );
  if ( downstreamLinks.isEmpty() )
    return nullptr;

  return downstreamLinks.at( 0 );
}

void ReosHydrographJunction::onUpstreamRoutingUpdated( const QString &routingId )
{
  mWaitingForUpstreamLinksUpdated.remove( routingId );
  mNeedCalculation = true;
  calculateIfAllReady();
}

void ReosHydrographJunction::calculateInternalHydrograph()
{
  mInternalHydrographUpdated = true;
  calculateIfAllReady();
}

void ReosHydrographJunction::calculateOuputHydrograph()
{
  mCalculationIsInProgress = true;

  if ( mSumCalculation )
    mSumCalculation->stop( true );

  HydrographSumCalculation *newCalculation = new HydrographSumCalculation;
  mSumCalculation = newCalculation;

  if ( !mInternalHydrograph.isNull() )
    newCalculation->addHydrograph( mInternalHydrograph );

  for ( const QPointer<ReosHydraulicLink> &link : std::as_const( mLinksBySide2 ) )
  {
    ReosHydrographRoutingLink *routing = qobject_cast<ReosHydrographRoutingLink *>( link );
    if ( routing )
    {
      ReosHydrograph *transferhydrograph = routing->outputHydrograph();
      if ( transferhydrograph )
        newCalculation->addHydrograph( transferhydrograph );
    }
  }

  if ( mUseForceOutputTimeStep->value() )
    newCalculation->forceOutputTimeStep( mForceOutputTimeStep->value() );

  connect( newCalculation, &ReosProcess::finished, this, [this, newCalculation]
  {
    if ( mSumCalculation == newCalculation )
    {
      mSumCalculation = nullptr;
      if ( newCalculation->isSuccessful() )
      {
        mOutputHydrograph->copyFrom( newCalculation->hydrograph() );
        mNeedCalculation = false;
        calculationUpdated();
      }
      mCalculationIsInProgress = false;
    }
    newCalculation->deleteLater();
  } );

  newCalculation->startOnOtherThread();
#ifndef _NDEBUG
  qDebug() << "calculation of junction: " << elementName()->value();
#endif
}

void ReosHydrographJunction::calculateIfAllReady()
{
  if ( mWaitingForUpstreamLinksUpdated.isEmpty() && mInternalHydrographUpdated && mNeedCalculation )
    calculateOuputHydrograph();
}

void ReosHydrographJunction::onInternalHydrographChanged()
{
  mNeedCalculation = true;
  calculateIfAllReady();
}

void ReosHydrographJunction::onTimeStepChange()
{
  mNeedCalculation = true;
  calculateIfAllReady();
}

ReosHydrographsStore *ReosHydrographJunction::gaugedHydrographsStore() const
{
  return mHydrographsStore;
}

QString ReosHydrographJunction::outputPrefixName() const
{
  return tr( "Output of " );
}

void ReosHydrographJunction::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  encodedElement.addData( QStringLiteral( "hydrograph-origin" ), int( mInternalHydrographOrigin ) );
  QString hydrographId;
  if ( mGaugedHydrographIndex >= 0 && mHydrographsStore->hydrograph( mGaugedHydrographIndex ) )
    hydrographId = mHydrographsStore->hydrograph( mGaugedHydrographIndex )->id();

  encodedElement.addData( QStringLiteral( "hydrograph-origin-id" ), hydrographId );

  scheme->saveElementConfig( id(), encodedElement );
}

void ReosHydrographJunction::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  QString hydrographId;
  encodedElement.getData( QStringLiteral( "hydrograph-origin-id" ), hydrographId );

  int hydrographCount = mHydrographsStore->hydrographCount();
  for ( int i = 0; i < hydrographCount; ++i )
  {
    if ( mHydrographsStore->hydrograph( i )->id() == hydrographId )
    {
      mGaugedHydrographIndex = i;
      break;
    }
  }

  int origin = static_cast<int>( mInternalHydrographOrigin );
  encodedElement.getData( QStringLiteral( "hydrograph-origin" ), origin );
  mInternalHydrographOrigin = static_cast<InternalHydrographOrigin>( origin );

  emit dataChanged();
}


ReosHydrograph *ReosHydrographJunction::internalHydrograph() const
{
  return mInternalHydrograph;
}

ReosHydrographNodeWatershed::ReosHydrographNodeWatershed( ReosWatershed *watershed,
    ReosMeteorologicModelsCollection *meteoModelCollection,
    ReosHydraulicNetwork *parent )
  : ReosHydrographJunction( QPointF(), parent )
  , mWatershed( watershed )
  , mRunoffHydrographs( new ReosRunoffHydrographsStore( meteoModelCollection, this ) )
{
  mInternalHydrographOrigin = RunoffHydrograph;
  if ( mWatershed )
    elementName()->setValue( mWatershed->watershedName()->value() );
  init();
}

ReosHydrographNodeWatershed::ReosHydrographNodeWatershed( const ReosEncodedElement &encodedElement,
    ReosWatershed *watershed,
    ReosMeteorologicModelsCollection *meteoModelCollection,
    ReosHydraulicNetwork *parent )
  : ReosHydrographJunction( encodedElement, parent )
  , mWatershed( watershed )
  , mRunoffHydrographs( new ReosRunoffHydrographsStore( meteoModelCollection, this ) )
{
  init();
}

void ReosHydrographNodeWatershed::init()
{
  if ( mWatershed )
  {
    connect( mWatershed, &ReosWatershed::outletPositionChange, this, [this]
    {
      positionChanged();
    } );

    connect( mWatershed->gaugedHydrographs(), &ReosHydrographsStore::hydrographRemoved, this, [this]( int hydrographIndex )
    {
      if ( mGaugedHydrographIndex == hydrographIndex )
      {
        mGaugedHydrographIndex = -1;
        updateInternalHydrograph();
      }
    } );
  }

  mRunoffHydrographs->setWatershed( mWatershed );
  if ( mHydrographsStore )
    delete mHydrographsStore;

  if ( mWatershed->watershedType() != ReosWatershed::Residual )
    mHydrographsStore = mWatershed->gaugedHydrographs();
  else
    mHydrographsStore = mWatershed->downstreamWatershed()->gaugedHydrographs();
}


ReosWatershed *ReosHydrographNodeWatershed::watershed() const
{
  if ( mWatershed.isNull() )
    return nullptr;
  else
    return mWatershed.data();
}

bool ReosHydrographNodeWatershed::updateInternalHydrographCalculationContext( const ReosCalculationContext &context )
{
  mLastMeteoModel = context.meteorologicModel();

  return updateInternalHydrograph();
}

void ReosHydrographJunction::setGaugedHydrographIndex( int gaugedHydrographIndex )
{
  if ( mGaugedHydrographIndex != gaugedHydrographIndex )
  {
    mGaugedHydrographIndex = gaugedHydrographIndex;
    emit dirtied();
  }

  updateInternalHydrograph();
}

void ReosHydrographNodeWatershed::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  QString watershedUri;
  if ( context.watershedModule() && context.watershedModule()->watershedTree() )
    watershedUri = context.watershedModule()->watershedTree()->watershedUri( mWatershed );
  element.addData( QStringLiteral( "watershed-uri" ), watershedUri );

  ReosHydrographJunction::encodeData( element, context );
}

bool ReosHydrographJunction::setCurrentInternalHydrograph( ReosHydrograph *newHydrograph )
{
  if ( mInternalHydrograph != newHydrograph )
  {
    if ( !mInternalHydrograph.isNull() )
      disconnect( mInternalHydrograph, &ReosDataObject::dataChanged, this, &ReosHydrographNodeWatershed::onInternalHydrographChanged );

    mInternalHydrograph = newHydrograph;
    emit internalHydrographPointerChange();

    mNeedCalculation = true;
    calculateInternalHydrograph();

    if ( newHydrograph )
      connect( mInternalHydrograph, &ReosDataObject::dataChanged, this, &ReosHydrographNodeWatershed::onInternalHydrographChanged );

    return true;
  }
  else
  {
    mInternalHydrographUpdated = true;
    calculateIfAllReady();
    return false;
  }
}

void ReosHydrographNodeWatershed::calculateInternalHydrograph()
{
  if ( mInternalHydrographOrigin == RunoffHydrograph && !mInternalHydrograph.isNull() && mInternalHydrograph->hydrographIsObsolete() )
  {
    connect( mRunoffHydrographs, &ReosRunoffHydrographsStore::hydrographReady, this, [this]( ReosHydrograph * updatedHydrograph )
    {
      if ( updatedHydrograph == mInternalHydrograph )
      {
        //mOutputHydrograph->copyFrom( updatedHydrograph );
        mInternalHydrographUpdated = true;
        calculateIfAllReady();
      }
    }, Qt::QueuedConnection ); //specify queued connection should be NOT necessary because mRunoffHydrographs emit the signal from an other thread

    mRunoffHydrographs->updateHydrograph( mInternalHydrograph );
    mInternalHydrographUpdated = false;
  }
  else if ( mInternalHydrograph )
  {
    mInternalHydrographUpdated = true;
    calculateIfAllReady();
  }
  else
  {
    mInternalHydrographUpdated = true;
  }
}

bool ReosHydrographNodeWatershed::updateInternalHydrograph()
{
  ReosHydrograph *newHydrograph = nullptr;

  switch ( mInternalHydrographOrigin )
  {
    case ReosHydrographNodeWatershed::RunoffHydrograph:
      newHydrograph = mRunoffHydrographs->hydrograph( mLastMeteoModel );
      break;
    case ReosHydrographNodeWatershed::GaugedHydrograph:
      newHydrograph = mWatershed->gaugedHydrographs()->hydrograph( mGaugedHydrographIndex );
      break;
  }

  return setCurrentInternalHydrograph( newHydrograph );
}

int ReosHydrographJunction::gaugedHydrographIndex() const
{
  return mGaugedHydrographIndex;
}

ReosHydrographNodeWatershed *ReosHydrographNodeWatershed::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydrographNodeWatershed::staticType() )
    return nullptr;

  QString watershedUri;
  encodedElement.getData( QStringLiteral( "watershed-uri" ), watershedUri );

  ReosWatershed *watershed = context.watershedModule()->watershedTree()->uriToWatershed( watershedUri );

  if ( !watershed )
    return nullptr;

  std::unique_ptr<ReosHydrographNodeWatershed> ret( new ReosHydrographNodeWatershed( encodedElement, watershed, context.watershedModule()->meteoModelsCollection(), context.network() ) );


  return ret.release();
}

void ReosHydrographJunction::setInternalHydrographOrigin( InternalHydrographOrigin origin )
{
  if ( origin == mInternalHydrographOrigin )
    return;

  mInternalHydrographOrigin = origin;
  emit dirtied();

  updateInternalHydrograph();
  calculateInternalHydrograph();
}

ReosHydrographJunction::InternalHydrographOrigin ReosHydrographJunction::internalHydrographOrigin() const
{
  return mInternalHydrographOrigin;
}


QPointF ReosHydrographNodeWatershed::position( const QString &destinationCrs ) const
{
  if ( mWatershed.isNull() )
    return QPointF();
  else
    return mWatershed->outletPoint();
}

ReosHydrographJunction::HydrographSumCalculation::HydrographSumCalculation()
{
}

void ReosHydrographJunction::HydrographSumCalculation::addHydrograph( ReosHydrograph *hydro )
{
  if ( !hydro )
    return;
  mHydrographsToAdd.append( new ReosHydrograph( this ) );
  mHydrographsToAdd.last()->copyFrom( hydro );
}

void ReosHydrographJunction::HydrographSumCalculation::forceOutputTimeStep( const ReosDuration &timeStep )
{
  mForceOutputTimeStep = true;
  mTimeStep = timeStep;
}

void ReosHydrographJunction::HydrographSumCalculation::start()
{
  mIsSuccessful = true;
  mHydrograph.reset( new ReosHydrograph );
  if ( mHydrographsToAdd.isEmpty() )
    return;

  mIsSuccessful = false;

  mHydrograph->copyFrom( mHydrographsToAdd.first() );

  setMaxProgression( mHydrographsToAdd.count() );

  for ( int i = 1; i < mHydrographsToAdd.count(); ++i )
  {
    if ( isStop() )
      break;

    setCurrentProgression( i );

    mHydrograph->addOther( mHydrographsToAdd.at( i ) );
  }

  if ( mForceOutputTimeStep &&
       mTimeStep > ReosDuration() &&
       mHydrograph->valueCount() > 0 )
  {
    std::unique_ptr<ReosHydrograph> hyd = std::make_unique<ReosHydrograph>();
    hyd->setReferenceTime( mHydrograph->referenceTime() );
    ReosDuration time( mHydrograph->relativeTimeAt( 0 ) );
    ReosDuration lastTime( mHydrograph->relativeTimeAt( mHydrograph->valueCount() - 1 ) );
    while ( time <= lastTime )
    {
      hyd->setValue( time, mHydrograph->valueAtTime( time ) );
      time = time + mTimeStep;
    }

    mHydrograph.reset( hyd.release() );
  }

  mIsSuccessful = !isStop();

}

ReosHydraulicNetworkElement *ReosHydrographNodeWatershedFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  return ReosHydrographNodeWatershed::decode( encodedElement, context );
}

ReosHydraulicNetworkElement *ReosHydrographJunctionFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  return ReosHydrographJunction::decode( encodedElement, context );
}
