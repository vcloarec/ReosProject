/***************************************************************************
  reoshydraulicnetwork.cpp - ReosHydraulicNetwork

 ---------------------
 begin                : 20.5.2021
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
#include "reoshydraulicnetwork.h"
#include "reoshydraulicnode.h"
#include "reoshydrauliclink.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydrographrouting.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoshydraulicscheme.h"
#include "reosgisengine.h"
#include <QUuid>

ReosHydraulicNetworkElement::ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent ):
  ReosDataObject( parent )
  , mNetwork( parent )
  , mNameParameter( new ReosParameterString( tr( "Name" ), false, this ) )
{
  mConstantTimeStepInTable = new ReosParameterDuration( tr( "Constant time step" ) );
  mConstantTimeStepInTable->setValue( ReosDuration( 5, ReosDuration::minute ) );
  mUseConstantTimeStepInTable = new ReosParameterBoolean( tr( "Display constant time step" ) );
  init();
}

ReosHydraulicNetworkElement::ReosHydraulicNetworkElement( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosDataObject( parent )
  , mNetwork( parent )
  , mNameParameter( ReosParameterString::decode( encodedElement.getEncodedData( QStringLiteral( "name-parameter" ) ), false, tr( "Name" ), this ) )
{
  ReosDataObject::decode( encodedElement );

  mConstantTimeStepInTable = ReosParameterDuration::decode( encodedElement.getEncodedData( QStringLiteral( "constant-time-step-in-table" ) ),
                             false,
                             tr( "Constant time step" ),
                             this );

  if ( !mConstantTimeStepInTable->isValid() )
    mConstantTimeStepInTable->setValue( ReosDuration( 5, ReosDuration::minute ) );

  mUseConstantTimeStepInTable = ReosParameterBoolean::decode( encodedElement.getEncodedData( QStringLiteral( "use-constant-time-step-in-table" ) ),
                                false,
                                tr( "Display constant time step" ),
                                this );
  if ( !mUseConstantTimeStepInTable->isValid() )
    mUseConstantTimeStepInTable->setValue( false );

  init();
}

void ReosHydraulicNetworkElement::init()
{
  connect( mConstantTimeStepInTable, &ReosParameter::valueChanged, this, &ReosHydraulicNetworkElement::dirtied );
  connect( mUseConstantTimeStepInTable, &ReosParameter::valueChanged, this, &ReosHydraulicNetworkElement::dirtied );
  connect( mNameParameter, &ReosParameter::valueChanged, this, &ReosHydraulicNetworkElement::dirtied );
}


ReosHydraulicNetworkElement::~ReosHydraulicNetworkElement()
{
}

void ReosHydraulicNetworkElement::destroy()
{
  if ( !mNetwork.isNull() )
    mNetwork->mElements.remove( id() );
  deleteLater();
}

void ReosHydraulicNetworkElement::positionChanged()
{
  if ( mNetwork )
    mNetwork->elemPositionChangedPrivate( this );
}

ReosParameterString *ReosHydraulicNetworkElement::elementName() const
{
  return mNameParameter;
}

ReosParameterDuration *ReosHydraulicNetworkElement::constantTimeStepInTable() const
{
  return mConstantTimeStepInTable;
}

ReosParameterBoolean *ReosHydraulicNetworkElement::useConstantTimeStepInTable() const
{
  return mUseConstantTimeStepInTable;
}

ReosEncodedElement ReosHydraulicNetworkElement::encode( const ReosHydraulicNetworkContext &context ) const
{
  ReosEncodedElement element( type() );
  element.addEncodedData( QStringLiteral( "name-parameter" ), mNameParameter->encode() );
  element.addEncodedData( QStringLiteral( "constant-time-step-in-table" ), mConstantTimeStepInTable->encode() );
  element.addEncodedData( QStringLiteral( "use-constant-time-step-in-table" ), mUseConstantTimeStepInTable->encode() );

  encodeData( element, context );

  ReosDataObject::encode( element );

  return element;
}

void ReosHydraulicNetworkElement::notify( const ReosModule::Message &messageObject )
{
  mLastMessage = messageObject;
  if ( !messageObject.text.isEmpty() )
  {
    ReosModule::Message sendedMessage = messageObject;
    sendedMessage.prefixMessage( tr( "Routing %1: " ).arg( elementName()->value() ) );
    if ( mNetwork )
      mNetwork->message( sendedMessage );
  }
}

ReosModule::Message ReosHydraulicNetworkElement::lastMessage() const
{
  return mLastMessage;
}

void ReosHydraulicNetworkElement::saveConfiguration( ReosHydraulicScheme * ) const {}

void ReosHydraulicNetworkElement::restoreConfiguration( ReosHydraulicScheme * ) {}

ReosDuration ReosHydraulicNetworkElement::currentElementTimeStep() const
{
  return ReosDuration( qint64( 0 ) );
}

ReosTimeWindow ReosHydraulicNetworkElement::timeWindow() const
{
  return ReosTimeWindow();
}

ReosDuration ReosHydraulicNetworkElement::mapTimeStep() const
{
  return ReosDuration();
}

ReosHydraulicNetwork *ReosHydraulicNetworkElement::network() const
{
  return mNetwork;
}

ReosMapExtent ReosHydraulicNetworkElement::extent() const
{
  return ReosMapExtent();
}

ReosHydraulicNetwork::ReosHydraulicNetwork( ReosModule *parent, ReosGisEngine *gisEngine, ReosWatershedModule *watershedModule )
  : ReosModule( parent )
  , mGisEngine( gisEngine )
  , mWatershedModule( watershedModule )
  , mHydraulicSchemeCollection( new ReosHydraulicSchemeCollection( this ) )
{
  ReosHydrographRoutingMethodFactories::instantiate( this );
  ReosGmshEngine::instantiate( this );

  mElementFactories.emplace( ReosHydrographNodeWatershed::staticType(), new ReosHydrographNodeWatershedFactory );
  mElementFactories.emplace( ReosHydrographJunction::staticType(), new ReosHydrographJunctionFactory );

  mElementFactories.emplace( ReosHydrographRoutingLink::staticType(), new ReosHydrographRoutingLinkFactory );

  mElementFactories.emplace( ReosHydraulicStructure2D::staticType(), new ReosHydraulicStructure2dFactory );
  mElementFactories.emplace( ReosHydraulicStructureBoundaryCondition::staticType(), new ReosHydraulicStructureBoundaryConditionFactory );

  std::unique_ptr<ReosHydraulicScheme> scheme = std::make_unique<ReosHydraulicScheme>();
  scheme->schemeName()->setValue( tr( "Hydraulic scheme" ) );
  if ( mWatershedModule && mWatershedModule->meteoModelsCollection()->modelCount() != 0 )
    scheme->setMeteoModel( mWatershedModule->meteoModelsCollection()->meteorologicModel( 0 ) );

  mHydraulicSchemeCollection->addScheme( scheme.release() );

  connect( mHydraulicSchemeCollection, &ReosHydraulicSchemeCollection::dirtied, this, &ReosModule::dirtied );
}

QList<ReosHydraulicNetworkElement *> ReosHydraulicNetwork::getElements( const QString &type ) const
{
  QList<ReosHydraulicNetworkElement *> elems;
  for ( ReosHydraulicNetworkElement *elem : mElements )
    if ( elem->type() == type )
      elems.append( elem );

  return elems;
}

ReosHydraulicNetworkElement *ReosHydraulicNetwork::getElement( const QString &elemId ) const
{
  if ( mElements.contains( elemId ) )
    return mElements.value( elemId );
  else
    return nullptr;
}

ReosHydraulicNetworkElement *ReosHydraulicNetwork::addElement( ReosHydraulicNetworkElement *elem, bool select )
{
  mElements.insert( elem->id(), elem );
  if ( !elem->elementName()->isValid() )
  {
    int index = mElementIndexesCounter.value( elem->type(), 0 ) + 1;
    mElementIndexesCounter[ elem->type()] = index;
    elem->elementName()->setValue( ( elem->defaultDisplayName() + QStringLiteral( " %1" ) ).arg( index ) );
  }
  emit elementAdded( elem, select );

  connect( elem, &ReosHydraulicNetworkElement::dirtied, this, &ReosModule::dirtied );
  connect( elem, &ReosHydraulicNetworkElement::timeStepChanged, this, &ReosHydraulicNetwork::timeStepChanged );

  return elem;
}

void ReosHydraulicNetwork::removeElement( ReosHydraulicNetworkElement *elem )
{
  if ( !elem )
    return;
  emit elementRemoved( elem );
  ReosHydraulicNode *node = qobject_cast<ReosHydraulicNode *>( elem );
  if ( node )
  {
    const QList<ReosHydraulicLink *> links = node->links();
    for ( ReosHydraulicLink *link :  links )
      removeElement( link );
  }

  ReosHydraulicStructure2D *structure = qobject_cast<ReosHydraulicStructure2D *>( elem );
  if ( structure )
  {
    for ( ReosHydraulicStructureBoundaryCondition *bc : structure->boundaryConditions() )
      removeElement( bc );
  }

  elem->destroy();

  emit dirtied();
}

void ReosHydraulicNetwork::decode( const ReosEncodedElement &element, const QString &projectPath, const QString &projectFileName )
{
  clear();
  if ( element.description() != QStringLiteral( "hydraulic-network" ) )
    return;

  mProjectName = projectFileName;
  mProjectPath = projectPath;

  element.getData( QStringLiteral( "elements-counter" ), mElementIndexesCounter );

  QList<ReosEncodedElement> encodedElements = element.getListEncodedData( QStringLiteral( "hydraulic-element" ) );

  //here order of adding element is important (nodes before structures before links, links need taht nodes exists before)

  for ( const ReosEncodedElement &encodedElement : encodedElements )
  {
    if ( encodedElement.description().contains( ReosHydraulicNode::staticType() ) )
    {
      addEncodedElement( encodedElement );
    }
  }

  for ( const ReosEncodedElement &encodedElement : encodedElements )
  {
    if ( encodedElement.description().contains( ReosHydraulicStructure2D::staticType() ) )
    {
      addEncodedElement( encodedElement );
    }
  }

  for ( const ReosEncodedElement &encodedElement : encodedElements )
  {
    if ( encodedElement.description().contains( ReosHydraulicLink::staticType() ) )
    {
      addEncodedElement( encodedElement );
    }
  }

  for ( ReosHydraulicNetworkElement *elem : std::as_const( mElements ) )
    elemPositionChangedPrivate( elem );

  mHydraulicSchemeCollection->decode( element.getEncodedData( QStringLiteral( "hydraulic-scheme-collection" ) ), context() );

  if ( mHydraulicSchemeCollection->schemeCount() == 0 )
  {
    std::unique_ptr<ReosHydraulicScheme> scheme = std::make_unique<ReosHydraulicScheme>();
    scheme->schemeName()->setValue( tr( "Hydraulic scheme" ) );
    scheme->setMeteoModel( mWatershedModule->meteoModelsCollection()->meteorologicModel( 0 ) );
    mHydraulicSchemeCollection->addScheme( scheme.release() );
  }

  element.getData( QStringLiteral( "current-scheme-index" ), mCurrentSchemeIndex );

  if ( mCurrentSchemeIndex >= mHydraulicSchemeCollection->schemeCount() )
    mCurrentSchemeIndex = mHydraulicSchemeCollection->schemeCount() > 0 ? 0 : -1;

  ReosHydraulicScheme *currentScheme = mHydraulicSchemeCollection->scheme( mCurrentSchemeIndex );
  if ( currentScheme )
  {
    mGisEngine->setTemporalRange( currentScheme->startTime()->value(), currentScheme->endTime()->value() );
    connect( currentScheme, &ReosDataObject::dataChanged, this, &ReosHydraulicNetwork::schemeChanged );
    connect( currentScheme, &ReosHydraulicScheme::timeExtentChanged, mGisEngine, &ReosGisEngine::setTemporalRange );

    for ( ReosHydraulicNetworkElement *elem :  std::as_const( mElements ) )
      elem->restoreConfiguration( currentScheme );
  }

  emit loaded();
}

ReosEncodedElement ReosHydraulicNetwork::encode( const QString &projectPath, const QString &projectFileName ) const
{
  ReosEncodedElement element( QStringLiteral( "hydraulic-network" ) );

  QList<ReosEncodedElement> encodedElements;
  mProjectName = projectFileName;
  mProjectPath = projectPath;

  for ( ReosHydraulicNetworkElement *elem : mElements )
  {
    encodedElements.append( elem->encode( context() ) );
  }

  element.addListEncodedData( QStringLiteral( "hydraulic-element" ), encodedElements );
  element.addData( QStringLiteral( "elements-counter" ), mElementIndexesCounter );

  ReosHydraulicScheme *currentScheme = mHydraulicSchemeCollection->scheme( mCurrentSchemeIndex );
  if ( currentScheme )
  {
    for ( ReosHydraulicNetworkElement *elem : std::as_const( mElements ) )
      elem->saveConfiguration( currentScheme );
  }

  element.addEncodedData( QStringLiteral( "hydraulic-scheme-collection" ), mHydraulicSchemeCollection->encode() );
  element.addData( QStringLiteral( "current-scheme-index" ), mCurrentSchemeIndex );

  return element;
}

void ReosHydraulicNetwork::clear()
{
  qDeleteAll( mElements );
  mElements.clear();
  mHydraulicSchemeCollection->clear();
  mElementIndexesCounter.clear();
  mCurrentSchemeIndex = -1;
  emit hasBeenReset();
}

void ReosHydraulicNetwork::reset()
{
  qDeleteAll( mElements );
  mElements.clear();
  ReosMeteorologicModel *meteoModel = nullptr;
  if ( mWatershedModule->meteoModelsCollection()->modelCount() > 0 )
    meteoModel = mWatershedModule->meteoModelsCollection()->meteorologicModel( 0 );

  mHydraulicSchemeCollection->reset( meteoModel );

  mElementIndexesCounter.clear();
  mCurrentSchemeIndex = 0;
  emit hasBeenReset();
}

ReosGisEngine *ReosHydraulicNetwork::gisEngine() const
{
  return mGisEngine;
}


void ReosHydraulicNetwork::elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem )
{
  emit elementPositionHasChanged( elem );
  emit dirtied();
}

ReosHydraulicNetworkContext ReosHydraulicNetwork::context() const
{
  ReosHydraulicNetworkContext context;
  context.mWatershedModule = mWatershedModule;
  context.mNetwork = const_cast<ReosHydraulicNetwork *>( this );
  context.mProjectName = mProjectName;
  context.mProjectPath = mProjectPath;
  return context;
}

ReosCalculationContext ReosHydraulicNetwork::calculationContext() const
{
  ReosCalculationContext context;
  ReosHydraulicScheme *currentScheme = mHydraulicSchemeCollection->scheme( mCurrentSchemeIndex );
  if ( currentScheme )
  {
    context.setMeteorologicModel( currentScheme->meteoModel() );
    context.setSimulationStartTime( currentScheme->startTime()->value() );
    context.setSimulationEndTime( currentScheme->endTime()->value() );
    context.setSchemeId( currentScheme->id() );
  }

  return context;
}

ReosHydraulicSchemeCollection *ReosHydraulicNetwork::hydraulicSchemeCollection() const
{
  return mHydraulicSchemeCollection;
}

void ReosHydraulicNetwork::changeScheme( int newSchemeIndex )
{
  if ( newSchemeIndex == mCurrentSchemeIndex )
    return;

  if ( mCurrentSchemeIndex >= 0 )
  {
    for ( ReosHydraulicNetworkElement *elem : std::as_const( mElements ) )
    {
      ReosHydraulicScheme *scheme = mHydraulicSchemeCollection->scheme( mCurrentSchemeIndex );
      if ( scheme )
        elem->saveConfiguration( scheme );
    }
  }

  setCurrentScheme( newSchemeIndex );
}

int ReosHydraulicNetwork::currentSchemeIndex() const
{
  return mCurrentSchemeIndex;
}

ReosHydraulicScheme *ReosHydraulicNetwork::currentScheme() const
{
  return mHydraulicSchemeCollection->scheme( mCurrentSchemeIndex );
}

void ReosHydraulicNetwork::setCurrentScheme( int newSchemeIndex )
{
  ReosHydraulicScheme *currentScheme = mHydraulicSchemeCollection->scheme( mCurrentSchemeIndex );
  if ( currentScheme )
  {
    disconnect( currentScheme, &ReosDataObject::dataChanged, this, &ReosHydraulicNetwork::schemeChanged );
    disconnect( currentScheme, &ReosHydraulicScheme::timeExtentChanged, mGisEngine, &ReosGisEngine::setTemporalRange );
  }

  mCurrentSchemeIndex = newSchemeIndex;

  for ( ReosHydraulicNetworkElement *elem :  std::as_const( mElements ) )
    elem->restoreConfiguration( mHydraulicSchemeCollection->scheme( newSchemeIndex ) );

  currentScheme = mHydraulicSchemeCollection->scheme( mCurrentSchemeIndex );
  if ( currentScheme )
  {
    mGisEngine->setTemporalRange( currentScheme->startTime()->value(), currentScheme->endTime()->value() );
    connect( currentScheme, &ReosDataObject::dataChanged, this, &ReosHydraulicNetwork::schemeChanged );
    connect( currentScheme, &ReosHydraulicScheme::timeExtentChanged, mGisEngine, &ReosGisEngine::setTemporalRange );
  }

  emit schemeChanged();
  emit dirtied();
}

ReosDuration ReosHydraulicNetwork::currentTimeStep() const
{
  ReosDuration ret( qint64( 0 ) );

  ReosHydraulicScheme *scheme = currentScheme();
  if ( scheme )
  {
    for ( ReosHydraulicNetworkElement *elem : mElements )
    {
      ReosDuration elemTs = elem->currentElementTimeStep();
      if ( elemTs != ReosDuration( qint64( 0 ) ) && ( ret > elemTs || ret == ReosDuration( qint64( 0 ) ) ) )
        ret = elemTs;
    }
  }

  return ret;
}

ReosMapExtent ReosHydraulicNetwork::networkExtent() const
{
  ReosMapExtent extent;
  for ( const ReosHydraulicNetworkElement *elem : mElements )
    extent.expendWithExtent( elem->extent() );

  return extent;
}

void ReosHydraulicNetwork::addEncodedElement( const ReosEncodedElement &element )
{
  auto it = mElementFactories.find( element.description() );
  if ( it == mElementFactories.end() )
    return;

  it->second->decodeElement( element, context() );
}

ReosWatershedModule *ReosHydraulicNetworkContext::watershedModule() const
{
  return mWatershedModule;
}

ReosHydraulicNetwork *ReosHydraulicNetworkContext::network() const
{
  return mNetwork;
}

QString ReosHydraulicNetworkContext::crs() const
{
  if ( mNetwork->gisEngine() )
    return mNetwork->gisEngine()->crs();

  return QString();
}

QString ReosHydraulicNetworkContext::projectPath() const
{
  return mProjectPath;
}

QString ReosHydraulicNetworkContext::projectName() const
{
  return mProjectName;
}

ReosEncodeContext ReosHydraulicNetworkContext::encodeContext() const
{
  ReosEncodeContext context;
  context.setBaseDir( QDir( mProjectPath ) );
  return context;
}

QString ReosHydraulicNetworkContext::currentSchemeId() const
{
  if ( mNetwork && mNetwork->currentScheme() )
    return mNetwork->currentScheme()->id();
  return QString();
}

ReosHydraulicNetworkElementFactory::ReosHydraulicNetworkElementFactory()
{

}

ReosHydraulicNetworkElementFactory::~ReosHydraulicNetworkElementFactory() {}
