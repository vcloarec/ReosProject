/***************************************************************************
  reoshydraulicnetworkwidget.cpp - ReosHydraulicNetworkWidget

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
#include "reoshydraulicnetworkwidget.h"
#include "ui_reoshydraulicnetworkwidget.h"

#include <QMessageBox>

#include "reoshydrographsource.h"
#include "reoshydrographrouting.h"
#include "reoshydrauliquestructure2d.h"
#include "reoswatershed.h"
#include "reosmaptool.h"
#include "reoshydraulicelementpropertieswidget.h"

ReosHydraulicNetworkWidget::ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, ReosWatershedModule *watershedModule, const ReosGuiContext &context ) :
  QWidget( context.parent() )
  , ui( new Ui::ReosHydraulicNetworkWidget )
  , mHydraulicNetwork( network )
  , mMap( context.map() )
  , mActionSelectNetworkElement( new QAction( QPixmap( QStringLiteral( ":/images/selectHydraulicElement.svg" ) ), tr( "Select Hydraulic Network Element" ), this ) )
  , mMapToolSelectNetworkElement( new ReosMapToolSelectMapItem( context.map(), ReosHydraulicNetworkElement::staticType() ) )
  , mActionAddHydrographJunction( new QAction( QPixmap( QStringLiteral( ":/images/addHydrographJunction.svg" ) ), tr( "Add Junction" ), this ) )
  , mMapToolAddHydrographJunction( new ReosMapToolDrawPoint( mMap ) )
  , mActionAddHydrographRouting( new QAction( QPixmap( QStringLiteral( ":/images/addHydrographRouting.svg" ) ), tr( "Add Link" ), this ) )
  , mMapToolAddHydrographRouting( new ReosMapToolDrawHydrographRouting( mHydraulicNetwork, mMap ) )
  , mActionHydraulicNetworkProperties( new QAction( QPixmap( QStringLiteral( ":/images/hydraulicProperties.svg" ) ), tr( "Hydraulic Element Properties" ), this ) )
  , mElementPropertiesWidget( new ReosHydraulicElementPropertiesActionWidget( watershedModule, ReosGuiContext( context, this ) ) )
  , mActionMoveHydrographJunction( new QAction( QPixmap( QStringLiteral( ":/images/moveHydrographJunction.svg" ) ),  tr( "Move Junction" ), this ) )
  , mMapToolMoveHydrographJunction( new ReosMapToolMoveHydraulicNetworkElement( network, context.map() ) )
  , mActionNewStructure2D( new QAction( tr( "Structure 2D" ), this ) )
  , mMapToolNewStructure2D( new ReosMapToolNewStructure2D( network, mMap ) )
  , mActionRemoveElement( new QAction( QPixmap( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Hydraulic Element" ), this ) )
{
  ui->setupUi( this );

  ui->mNameWidget->setDefaultName( tr( "Name" ) );

  ReosHydraulicNetworkDockWidget *dockParent = qobject_cast<ReosHydraulicNetworkDockWidget *>( context.parent() );
  if ( dockParent )
  {
    connect( dockParent, &ReosHydraulicNetworkDockWidget::shown, this, &ReosHydraulicNetworkWidget::onOpened );
    connect( dockParent, &ReosHydraulicNetworkDockWidget::closed, this, &ReosHydraulicNetworkWidget::onClosed );
  }

  QToolBar *toolBar = new QToolBar( this );
  ui->mainLayout->insertWidget( 0, toolBar );

  toolBar->addAction( mActionSelectNetworkElement );
  mActionSelectNetworkElement->setCheckable( true );
  mMapToolSelectNetworkElement->setAction( mActionSelectNetworkElement );
  mMapToolSelectNetworkElement->setSearchItemWhenMoving( true );
  mMapToolSelectNetworkElement->setCursor( Qt::ArrowCursor );

  toolBar->addAction( mActionAddHydrographJunction );
  mActionAddHydrographJunction->setCheckable( true );
  mMapToolAddHydrographJunction->setAction( mActionAddHydrographJunction );

  toolBar->addAction( mActionAddHydrographRouting );
  mActionAddHydrographRouting->setCheckable( true );
  mMapToolAddHydrographRouting->setAction( mActionAddHydrographRouting );
  mMapToolAddHydrographRouting->setSearchingItemDecription( ReosHydrographSource::staticType() );
  mMapToolAddHydrographRouting->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionNewStructure2D );
  mActionNewStructure2D->setCheckable( true );
  mMapToolNewStructure2D->setAction( mActionNewStructure2D );

  toolBar->addAction( mActionMoveHydrographJunction );
  mActionMoveHydrographJunction->setCheckable( true );
  mMapToolMoveHydrographJunction->setAction( mActionMoveHydrographJunction );
  mMapToolMoveHydrographJunction->setSearchingItemDecription( ReosHydrographJunction::staticType() );
  mMapToolMoveHydrographJunction->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionHydraulicNetworkProperties );
  mActionHydraulicNetworkProperties->setCheckable( true );
  mElementPropertiesWidget->setAction( mActionHydraulicNetworkProperties );

  toolBar->addAction( mActionRemoveElement );
  mActionRemoveElement->setEnabled( false );

  connect( network, &ReosHydraulicNetwork::hasBeenReset, this, &ReosHydraulicNetworkWidget::onModuleReset );

  connect( mActionRemoveElement, &QAction::triggered, this, &ReosHydraulicNetworkWidget::onSelectedElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosHydraulicNetworkWidget::onElementAdded );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementRemoved, this, &ReosHydraulicNetworkWidget::onElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementPositionHasChanged, this, &ReosHydraulicNetworkWidget::onElementChanged );

  connect( mMapToolAddHydrographJunction, &ReosMapToolDrawPoint::drawn, this, [this]( const QPointF & p )
  {
    ReosHydraulicNetworkElement *elem = mHydraulicNetwork->addElement( new ReosHydrographJunction( p, mHydraulicNetwork ) );
    onElementSelected( mMapItems.value( elem ).get() );
  } );

  connect( mMapToolAddHydrographRouting, &ReosMapToolDrawHydrographRouting::finished, this, &ReosHydraulicNetworkWidget::onDrawHydrographRoutingFinish );

  connect( mMapToolSelectNetworkElement, &ReosMapToolSelectMapItem::found, this, &ReosHydraulicNetworkWidget::onElementSelected );

  onElementSelected( nullptr );
}

ReosHydraulicNetworkWidget::~ReosHydraulicNetworkWidget()
{
  delete ui;
}

void ReosHydraulicNetworkWidget::onElementAdded( ReosHydraulicNetworkElement *elem )
{
  NetworkItem item;
  item.reset( mMapItemFactory.createMapItem( elem, mMap ) );
  if ( item )
    mMapItems[elem] =  item ;

  addGeometryStructure( elem );

}

void ReosHydraulicNetworkWidget::onElementRemoved( ReosHydraulicNetworkElement *elem )
{
  removeGeometryStructure( elem );
  mMapItems.remove( elem );
  onElementSelected( nullptr );

}

void ReosHydraulicNetworkWidget::onElementChanged( ReosHydraulicNetworkElement *elem )
{
  auto it = mMapItems.constFind( elem );
  if ( it != mMapItems.constEnd() )
    mMapItemFactory.updateMapItem( elem, it.value().get() );
}

void ReosHydraulicNetworkWidget::onDrawHydrographRoutingFinish()
{
  QList<ReosMapItem *> itemList = mMapToolAddHydrographRouting->linkedItems();
  Q_ASSERT( itemList.count() == 2 );

  ReosHydrographSource *source = qobject_cast<ReosHydrographSource *>( mHydraulicNetwork->getElement( itemList.at( 0 )->description() ) );
  Q_ASSERT( source );

  ReosHydrographNode *destination = qobject_cast<ReosHydrographNode *>( mHydraulicNetwork->getElement( itemList.at( 1 )->description() ) );
  Q_ASSERT( destination );

  ReosHydraulicNetworkElement *elem = mHydraulicNetwork->addElement( new ReosHydrographRoutingLink( source, destination, mHydraulicNetwork ) );

  onElementSelected( mMapItems.value( elem ).get() );
}

void ReosHydraulicNetworkWidget::onElementSelected( ReosMapItem *item )
{
  if ( mCurrentSelectedElement )
  {
    auto it = mMapItems.constFind( mCurrentSelectedElement );
    if ( it != mMapItems.constEnd() )
      mMapItemFactory.unselectItem( mCurrentSelectedElement, it.value().get() );
    mCurrentSelectedElement = nullptr;
  }

  mActionRemoveElement->setEnabled( item != nullptr );

  if ( !item )
  {
    mElementPropertiesWidget->setCurrentElement( nullptr );
    ui->mNameWidget->setString( nullptr );
    mExtraItemSelection.reset( );
    return;
  }

  ReosHydraulicNetworkElement *elem = mHydraulicNetwork->getElement( item->description() );
  if ( elem )
    mMapItemFactory.selectItem( elem, item );
  mCurrentSelectedElement = elem;

  mElementPropertiesWidget->setCurrentElement( elem );
  ui->mNameWidget->setString( elem->name() );
  mExtraItemSelection.reset( mMapItemFactory.createExtraItemSelected( elem, mMap ) );
}

void ReosHydraulicNetworkWidget::onSelectedElementRemoved()
{
  mMapToolSelectNetworkElement->clearHoveredItem();

  if ( !mCurrentSelectedElement )
    return;
  if ( QMessageBox::warning( this, tr( "Remove Hydraulic Network Element" ), tr( "This action will remove definitly the element \"%1\"\n"
                             "Do you want to proceed?" ).arg( mCurrentSelectedElement->name()->value() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
    return;

  if ( mCurrentSelectedElement )
    mHydraulicNetwork->removeElement( mCurrentSelectedElement );

  mCurrentSelectedElement = nullptr;
  mActionRemoveElement->setEnabled( false );
}

void ReosHydraulicNetworkWidget::onModuleReset()
{
  mCurrentSelectedElement = nullptr;
  mElementPropertiesWidget->setCurrentElement( nullptr );
  mMapItems.clear();
}

void ReosHydraulicNetworkWidget::onClosed()
{
  setMapItemVisible( false );
}

void ReosHydraulicNetworkWidget::onOpened()
{
  setMapItemVisible( true );
}

void ReosHydraulicNetworkWidget::setMapItemVisible( bool visible )
{
  for ( NetworkItem &item : mMapItems )
    item->setVisible( visible );

  if ( mExtraItemSelection )
    mExtraItemSelection->setVisible( visible );

  if ( visible )
    for ( ReosGeometryStructure *structure : std::as_const( mGeometryStructure ) )
      mMap->addSnappableStructure( structure );
  else
    for ( ReosGeometryStructure *structure : std::as_const( mGeometryStructure ) )
      mMap->removeSnappableStructure( structure );
}

void ReosHydraulicNetworkWidget::addGeometryStructure( ReosHydraulicNetworkElement *elem )
{
  if ( elem->type() == ReosHydraulicStructure2D::staticType() )
  {
    ReosGeometryStructure *structure = qobject_cast<ReosHydraulicStructure2D *>( elem )->structure();
    if ( structure )
    {
      if ( isVisible() )
        mMap->addSnappableStructure( structure );
      mGeometryStructure.append( structure );
    }
  }
}

void ReosHydraulicNetworkWidget::removeGeometryStructure( ReosHydraulicNetworkElement *elem )
{
  if ( elem->type() == ReosHydraulicStructure2D::staticType() )
  {
    ReosGeometryStructure *structure = qobject_cast<ReosHydraulicStructure2D *>( elem )->structure();
    if ( structure )
    {
      mMap->removeSnappableStructure( qobject_cast<ReosHydraulicStructure2D *>( elem )->structure() );
    }
    mGeometryStructure.removeOne( structure );
  }
}


ReosHydraulicElementWidget::ReosHydraulicElementWidget( QWidget *parent ):  QWidget( parent )
{}

ReosHydraulicNetworkDockWidget::ReosHydraulicNetworkDockWidget( ReosHydraulicNetwork *network, ReosWatershedModule *watershedModule, const ReosGuiContext &context )
  : ReosDockWidget( tr( "Hydraulic Network" ), context.parent() )
{
  setWidget( new ReosHydraulicNetworkWidget( network, watershedModule, ReosGuiContext( context, this ) ) );
}
