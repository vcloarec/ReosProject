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
#include "reoshydrographtransfer.h"
#include "reosstyleregistery.h"

ReosHydrographNode::ReosHydrographNode( ReosHydraulicNetwork *parent )
  : ReosHydraulicNode( parent )
{}

ReosHydrographNode::ReosHydrographNode( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydraulicNode( encodedElement, parent )
{}

ReosHydrographSource::ReosHydrographSource( ReosHydraulicNetwork *parent ) : ReosHydrographNode( parent )
{}

ReosHydrographSource::ReosHydrographSource( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydrographNode( encodedElement, parent )
{}

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

ReosHydrograph *ReosHydrographSourceFixed::outputHydrograph()
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

  init();
}

ReosHydrographJunction::ReosHydrographJunction( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydrographSource( encodedElement, parent )
  , mOutputHydrograph( new ReosHydrograph( this ) )
{
  encodedElement.getData( QStringLiteral( "position" ), mPosition );

  QColor outputColor;
  encodedElement.getData( QStringLiteral( "output-color" ), outputColor );
  mOutputHydrograph->setColor( outputColor );

  init();
}


void ReosHydrographJunction::init()
{
  mOutputHydrograph->setName( tr( "Output of %1" ).arg( name()->value() ) );
  connect( name(), &ReosParameterString::valueChanged, mOutputHydrograph, [this]
  {
    mOutputHydrograph->setName( tr( "Output of %1" ).arg( name()->value() ) );
  } );

  connect( this, &ReosHydraulicNode::dataChanged, this, [this]
  {
    mNeedCalculation = true;
    mOutputHydrograph->setHydrographObsolete();
  } );
}

ReosHydrograph *ReosHydrographJunction::outputHydrograph()
{
  return mOutputHydrograph;
}

QPointF ReosHydrographJunction::position() const
{
  return mPosition;
}

void ReosHydrographJunction::setPosition( const QPointF &pos )
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

void ReosHydrographJunction::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext & ) const
{
  element.addData( QStringLiteral( "position" ), mPosition );
  element.addData( QStringLiteral( "output-color" ), mOutputHydrograph->color() );
}

bool ReosHydrographJunction::updateInternalHydrographCalculationContext( const ReosCalculationContext & )
{
  mInternalHydrographUpdated = true;
  return false;
}


ReosHydrographJunction *ReosHydrographJunction::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydrographJunction::staticType() )
    return nullptr;

  QPointF position;
  encodedElement.getData( QStringLiteral( "position" ), position );

  std::unique_ptr<ReosHydrographJunction> ret( new ReosHydrographJunction( encodedElement, context.network() ) );

  return ret.release();
}

void ReosHydrographJunction::updateCalculationContext( const ReosCalculationContext &context )
{
  updateCalculationContextFromUpstream( context, nullptr, false ); //this lead to update all routine lined to this junction and also this junction
}

void ReosHydrographJunction::updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydrographRoutingLink *upstreamRoutine, bool upstreamWillChange )
{
  QList<ReosHydrographRoutingLink *> upstreamLinks = ReosHydraulicNetworkUtils::upstreamLinkOfType<ReosHydrographRoutingLink>( this );

  upstreamLinks.removeOne( upstreamRoutine );

  if ( upstreamWillChange )
    mWaitingForUpstreamLinksUpdated.insert( upstreamRoutine->id() );

  for ( ReosHydrographRoutingLink *routine : std::as_const( upstreamLinks ) )
  {
    if ( routine )
    {
      bool routineNeedToBeUpdated = routine->updateCalculationContextFromDownstream( context );
      if ( routineNeedToBeUpdated )
        mWaitingForUpstreamLinksUpdated.insert( routine->id() );
      mNeedCalculation |= routineNeedToBeUpdated;
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
  for ( ReosHydrographRoutingLink *routine : upstreamLinks )
  {
    if ( routine )
    {
      bool routineNeedToBeUpdated = routine->updateCalculationContextFromDownstream( context );
      mNeedCalculation |= routineNeedToBeUpdated;
      if ( routineNeedToBeUpdated )
        mWaitingForUpstreamLinksUpdated.insert( routine->id() );
    }
  }

  mNeedCalculation |= updateInternalHydrographCalculationContext( context ) || isObsolete();

  if ( mNeedCalculation )
  {
    calculateIfAllReady();
  }

  return mNeedCalculation;
}

ReosHydrographRoutingLink *ReosHydrographJunction::downstreamRoutine() const
{
  QList<ReosHydrographRoutingLink *> downstreamLinks = ReosHydraulicNetworkUtils::downstreamLinkOfType<ReosHydrographRoutingLink>( this );
  Q_ASSERT( downstreamLinks.count() < 2 );
  if ( downstreamLinks.isEmpty() )
    return nullptr;

  return downstreamLinks.at( 0 );
}

void ReosHydrographJunction::onUpstreamRoutineUpdated( const QString &routingId )
{
  mWaitingForUpstreamLinksUpdated.remove( routingId );
  mNeedCalculation = true;
  calculateIfAllReady();
}


void ReosHydrographJunction::calculateInternalHydrograph() {}


void ReosHydrographJunction::calculateOuputHydrograph()
{
  mCalculationIsInProgress = true;

  if ( mSumCalculation )
    mSumCalculation->stop( true );

  HydrographSumCalculation *newCalculation = new HydrographSumCalculation;
  mSumCalculation = newCalculation;

  if ( !mInternalHydrograph.isNull() )
    newCalculation->addHydrograph( mInternalHydrograph );

  for ( const QPointer<ReosHydraulicLink> &link : mLinksBySide2 )
  {
    ReosHydrographRoutingLink *routing = qobject_cast<ReosHydrographRoutingLink *>( link );
    if ( routing )
    {
      ReosHydrograph *transferhydrograph = routing->outputHydrograph();
      if ( transferhydrograph )
        newCalculation->addHydrograph( transferhydrograph );
    }
  }

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
  qDebug() << "calculation of junction: " << name()->value();
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
  if ( mWatershed )
    name()->setValue( mWatershed->watershedName()->value() );
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
  int origin;
  encodedElement.getData( QStringLiteral( "hydrograph-origin" ), origin );
  mOrigin = static_cast<ReosHydrographNodeWatershed::HydrographOrigin>( origin );
  encodedElement.getData( QStringLiteral( "gauged-hydrograph-index" ), mGaugedHydrographIndex );

  init();
}

void ReosHydrographNodeWatershed::init()
{
  if ( mWatershed )
  {
    connect( mWatershed, &ReosWatershed::dataChanged, this, [this]
    {
      positionChanged();
    } );

    connect( mWatershed->gaugedHydrographs(), &ReosHydrographsStore::hydrographRemoved, this, [this]( int hydrographIndex )
    {
      if ( mGaugedHydrographIndex == hydrographIndex )
        mGaugedHydrographIndex = -1;
    } );
  }


  mRunoffHydrographs->setWatershed( mWatershed );

//  //***** not sure necessary????
//  connect( mRunoffHydrographs, &ReosRunoffHydrographsStore::hydrographReady, this, [this]( ReosHydrograph * updatedHydrograph )
//  {
//    if ( mCurrentWatershedHydrograph == updatedHydrograph )
//      calculationUpdated();
//  } );
//  //*****
}

ReosHydrograph *ReosHydrographNodeWatershed::outputHydrograph()
{
  return mOutputHydrograph;
}

ReosWatershed *ReosHydrographNodeWatershed::watershed() const
{
  if ( mWatershed.isNull() )
    return nullptr;
  else
    return mWatershed.data();
}

//bool ReosHydrographNodeWatershed::updateCalculationContext( const ReosCalculationContext &context )
//{
//  bool ret = ReosHydrographJunction::updateCalculationContext( context );

//  mLastMeteoModel = context.meteorologicModel();

//  mNeedCalculation = ret | updateWatershedHydrograph();

//  return mNeedCalculation ;
//}


bool ReosHydrographNodeWatershed::updateInternalHydrographCalculationContext( const ReosCalculationContext &context )
{
  mLastMeteoModel = context.meteorologicModel();

  return updateWatershedHydrograph();
}

void ReosHydrographNodeWatershed::setGaugedHydrographIndex( int gaugedHydrographIndex )
{
  if ( mGaugedHydrographIndex != gaugedHydrographIndex )
  {
    mGaugedHydrographIndex = gaugedHydrographIndex;
  }

  if ( mOrigin == ReosHydrographNodeWatershed::GaugedHydrograph )
  {
    setCurrentWatershedHydrograph( mWatershed->gaugedHydrographs()->hydrograph( mGaugedHydrographIndex ) );
    calculateInternalHydrograph();
  }
}

void ReosHydrographNodeWatershed::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  QString watershedUri;
  if ( context.watershedModule() && context.watershedModule()->watershedTree() )
    watershedUri = context.watershedModule()->watershedTree()->watershedUri( mWatershed );
  element.addData( QStringLiteral( "watershed-uri" ), watershedUri );
  element.addData( QStringLiteral( "hydrograph-origin" ), int( mOrigin ) );
  element.addData( QStringLiteral( "gauged-hydrograph-index" ), mGaugedHydrographIndex );

  ReosHydrographJunction::encodeData( element, context );
}

bool ReosHydrographNodeWatershed::setCurrentWatershedHydrograph( ReosHydrograph *watershedHydrograph )
{
  if ( mInternalHydrograph != watershedHydrograph )
  {
    if ( !mInternalHydrograph.isNull() )
      disconnect( mInternalHydrograph, &ReosDataObject::dataChanged, this, &ReosHydrographNodeWatershed::onInternalHydrographChanged );

    mInternalHydrograph = watershedHydrograph;
    emit internalHydrographPointerChange();

    mNeedCalculation = true;
    calculateInternalHydrograph();

    if ( watershedHydrograph )
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
  if ( mOrigin == RunoffHydrograph && !mInternalHydrograph.isNull() && mInternalHydrograph->hydrographIsObsolete() )
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

bool ReosHydrographNodeWatershed::updateWatershedHydrograph()
{
  ReosHydrograph *newHydrograph = nullptr;

  switch ( mOrigin )
  {
    case ReosHydrographNodeWatershed::RunoffHydrograph:
      newHydrograph = mRunoffHydrographs->hydrograph( mLastMeteoModel );
      break;
    case ReosHydrographNodeWatershed::GaugedHydrograph:
      newHydrograph = mWatershed->gaugedHydrographs()->hydrograph( mGaugedHydrographIndex );
      break;
  }

  return setCurrentWatershedHydrograph( newHydrograph );
}

int ReosHydrographNodeWatershed::gaugedHydrographIndex() const
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

void ReosHydrographNodeWatershed::setOrigin( HydrographOrigin origin )
{
  if ( origin == mOrigin )
    return;

  mOrigin = origin;
  updateWatershedHydrograph();

  calculateInternalHydrograph();
}

ReosHydrographNodeWatershed::HydrographOrigin ReosHydrographNodeWatershed::origin() const
{
  return mOrigin;
}


QPointF ReosHydrographNodeWatershed::position() const
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
  mHydrographsToAdd.append( new ReosHydrograph( this ) );
  mHydrographsToAdd.last()->copyFrom( hydro );
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
