/***************************************************************************
                      reosvertexzspecifierwidget.cpp
                     --------------------------------------
Date                 : 01-10-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosvertexzspecifierwidget.h"
#include "ui_reosvertexzspecifierwidget.h"

ReosVertexZSpecifierWidget::ReosVertexZSpecifierWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosVertexZSpecifierWidget )
{
  ui->setupUi( this );
  addEntry( new ReosVertexZSpecifierSimpleValueWidget( this ) );
  addEntry( new ReosVertexZSpecifierSlopeWidget( map, domain, this ) );
  addEntry( new ReosVertexZSpecifierGapWidget( map, domain, this ) );

  for ( auto entry : mEntryWidgets )
    entry->hide();

  mEntriesModel = new ReosVertexZSpecifierEntryWidgetModel( mEntryWidgets, this );
  ui->mEntriesListView->setModel( mEntriesModel );
  setCurrentZSpecifier( 0 );

  connect( ui->mEntriesListView, &QListView::clicked, this, &ReosVertexZSpecifierWidget::listViewClicked );

}

ReosVertexZSpecifierWidget::~ReosVertexZSpecifierWidget()
{
  delete ui;
}

void ReosVertexZSpecifierWidget::assignZSpecifier( VertexPointer vert )
{
  if ( mCurrentEntryWidget )
    vert->setZSpecifier( mCurrentEntryWidget->factory() );
}

void ReosVertexZSpecifierWidget::start()
{
  show();
  if ( mCurrentEntryWidget )
    mCurrentEntryWidget->start();
}

void ReosVertexZSpecifierWidget::stop()
{
  hide();
}

void ReosVertexZSpecifierWidget::listViewClicked( QModelIndex index )
{
  setCurrentZSpecifier( index.row() );
}

void ReosVertexZSpecifierWidget::setCurrentZSpecifier( int i )
{
  if ( mCurrentEntryWidget == mEntryWidgets.at( i ) )
    return;

  if ( mCurrentEntryWidget )
    mCurrentEntryWidget->stop();

  ui->mEntriesListView->setCurrentIndex( mEntriesModel->index( i ) );
  mCurrentEntryWidget = mEntryWidgets.at( i );
  mCurrentEntryWidget->start();

}

void ReosVertexZSpecifierWidget::addEntry( ReosVertexZSpecifierEntryWidget *entry )
{
  mEntryWidgets.append( entry );
  ui->widgetSpecificZSpecifier->layout()->addWidget( entry );
}

ReosVertexZSpecifierEntryWidget::ReosVertexZSpecifierEntryWidget( QWidget *parent ): QWidget( parent ) {}

ReosVertexZSpecifierEntryWidget::~ReosVertexZSpecifierEntryWidget() {}

void ReosVertexZSpecifierEntryWidget::start()
{
  show();
}

void ReosVertexZSpecifierEntryWidget::stop()
{
  hide();
}

ReosVertexZSpecifierDependentOtherVertexWidget::ReosVertexZSpecifierDependentOtherVertexWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent ): ReosVertexZSpecifierEntryWidget( parent ),
  mMap( map ), mDomain( domain )
{

}

void ReosVertexZSpecifierDependentOtherVertexWidget::init()
{
  mValueForm = new ReosForm( this );
  mValueForm->setOrientation( Qt::Vertical );

  ReosForm *referenceForm = new ReosForm( mValueForm );
  mValueForm->addForm( referenceForm );
  referenceForm->setOrientation( Qt::Horizontal );


  mSelectReferenceAction = new ReosFormAction( referenceForm, QIcon( QPixmap( "://toolbar/ZSpecifierSelectDependingVertex.png" ) ), tr( "Select reference vertex" ) );
  mSelectReferenceAction->setCheckable( true );
  mReferenceText = new ReosFormText( referenceForm, "" );

  mValueParameterForm = makeValueParameterForm( mValueForm );

  mTakeNewVertexAsReference = new ReosFormParameterSimpleBool( tr( "Take last new vertex as reference " ), false, mValueForm );

  setLayout( new QHBoxLayout );
  layout()->addWidget( mValueForm->getWidget() );

  selectReferenceMapTool = new ReosVertexZSpecifierSelectReferenceMapTool( mMap, mDomain );
  selectReferenceMapTool->setAction( mSelectReferenceAction->action() );

  connect( mSelectReferenceAction->action(), &QAction::triggered, this, &ReosVertexZSpecifierDependentOtherVertexWidget::startSelectReference );
  connect( selectReferenceMapTool, &ReosMapToolSelection::zonalCanvasRect, this, &ReosVertexZSpecifierDependentOtherVertexWidget::zoneHasBeenSelected );
  connect( mValueParameterForm, &ReosFormParameterSimpleDouble::valueEdited, this, &ReosVertexZSpecifierDependentOtherVertexWidget::valueHasBeenEdited );

  updateReferenceText();
}



ReosVertexZSpecifierFactory &ReosVertexZSpecifierDependentOtherVertexWidget::factory() {return dependentVertexFactory();}

void ReosVertexZSpecifierDependentOtherVertexWidget::start()
{
  updateReferenceText();
  show();
  if ( dependentVertexFactory().otherVertex() == nullptr )
  {
    startSelectReference();
  }
}

void ReosVertexZSpecifierDependentOtherVertexWidget::stop()
{
  hide();
  if ( mMap->getMaptool() == selectReferenceMapTool )
    selectReferenceMapTool->returnToPreviousMapTool();
}

void ReosVertexZSpecifierDependentOtherVertexWidget::startSelectReference()
{
  selectReferenceMapTool->start();
}

void ReosVertexZSpecifierDependentOtherVertexWidget::updateReferenceText()
{
  if ( dependentVertexFactory().otherVertex() == nullptr )
  {
    mReferenceText->setText( tr( "no reference" ).prepend( "<b><font color=\"red\">" ).append( "</font></b>" ) );
  }
  else
  {
    mReferenceText->setText( tr( "Reference coordinates" ).append( QString( " : <font color=\"green\"><b>X= %1 , Y= %2</font></b>" ).
                             arg( QString::number( dependentVertexFactory().otherVertex()->x(), 'f', 2 ) ).
                             arg( QString::number( dependentVertexFactory().otherVertex()->y(), 'f', 2 ) ) ) );
  }
}

void ReosVertexZSpecifierDependentOtherVertexWidget::zoneHasBeenSelected( const QRectF &rect )
{
  auto vert = mDomain->vertex( rect );
  if ( vert )
  {
    dependentVertexFactory().setOtherVertex( vert->realWorldVertex() );
    updateReferenceText();
    selectReferenceMapTool->returnToPreviousMapTool();
  }
}

void ReosVertexZSpecifierDependentOtherVertexWidget::valueHasBeenEdited()
{
  setValueInFactory( mValueParameterForm->getValue() );
}

ReosVertexZSpecifierSimpleValueWidget::ReosVertexZSpecifierSimpleValueWidget( QWidget *parent ): ReosVertexZSpecifierEntryWidget( parent )
{
  valueForm = new ReosForm( this );
  zValueParameterForm = new ReosFormParameterSimpleDouble( tr( "Z value : " ), 0, nullptr, " m" );
  valueForm->addParamater( zValueParameterForm );
  setLayout( new QHBoxLayout );
  layout()->addWidget( valueForm->getWidget() );

  connect( zValueParameterForm, &ReosFormParameterSimpleDouble::valueEdited, this, &ReosVertexZSpecifierSimpleValueWidget::ZValueHasBeenEdited );
}

QIcon ReosVertexZSpecifierSimpleValueWidget::icon() const
{
  return QIcon( QPixmap( "://toolbar/ZSpecifierSimpleValue.png" ) );
}

ReosVertexZSpecifierFactory &ReosVertexZSpecifierSimpleValueWidget::factory() {return mFactory;}

void ReosVertexZSpecifierSimpleValueWidget::ZValueHasBeenEdited()
{
  mFactory.setZValue( zValueParameterForm->getValue() );
}

ReosVertexZSpecifierSelectReferenceMapTool::ReosVertexZSpecifierSelectReferenceMapTool( ReosMap *map, ReosMapMeshEditorItemDomain *domain ): ReosMapToolSelection( map ), mDomain( domain )
{
  setCursor( QCursor( QPixmap( "://selectingDependingVertex.png" ), 0, 0 ) );
}

void ReosVertexZSpecifierSelectReferenceMapTool::start()
{
  mPreviousCurrentMapTool = map()->getMaptool();
  if ( mPreviousCurrentMapTool )
    mPreviousCurrentMapTool->suspend();
  map()->setMapTool( this );
}

void ReosVertexZSpecifierSelectReferenceMapTool::returnToPreviousMapTool()
{
  if ( mPreviousCurrentMapTool )
  {
    mPreviousCurrentMapTool->unsuspend();
    map()->setMapTool( mPreviousCurrentMapTool );
  }

}

void ReosVertexZSpecifierSelectReferenceMapTool::canvasMoveEvent( QgsMapMouseEvent *e )
{
  QPointF canvasPoint = toCanvasCoordinates( e->mapPoint() );
  ReosMeshItemVertex *vert = mDomain->vertex( selectedZone( canvasPoint ) );
  if ( !vert )
    setCursor( QCursor( QPixmap( "://selectingDependingVertex.png" ), 0, 0 ) );
  else
  {
    setCursor( QCursor( QPixmap( "://selectedDependingVertex.png" ), 0, 0 ) );
  }
}

void ReosVertexZSpecifierSelectReferenceMapTool::askForEscape()
{
  if ( mPreviousCurrentMapTool && mPreviousCurrentMapTool->isInProgress() )
  {
    mPreviousCurrentMapTool->askForEscape();
  }
  else
  {
    emit stop();
    if ( mPreviousCurrentMapTool )
      mPreviousCurrentMapTool->askForEscape();
  }
}

ReosVertexZSpecifierSlopeWidget::ReosVertexZSpecifierSlopeWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent ): ReosVertexZSpecifierDependentOtherVertexWidget( map, domain, parent )
{
  init();
}

QIcon ReosVertexZSpecifierSlopeWidget::icon() const
{
  return QIcon( QPixmap( "://toolbar/ZSpecifierVertexAndSlope.png" ) );
}

ReosFormParameterSimpleDouble *ReosVertexZSpecifierSlopeWidget::makeValueParameterForm( ReosForm *parentForm )
{
  return new ReosFormParameterSimpleDouble( tr( "Slope : " ), 0, parentForm, " %" );
}

void ReosVertexZSpecifierSlopeWidget::setValueInFactory( double value )
{
  mFactory.setSlope( value / 100 );
}

ReosVertexZSpecifierGapWidget::ReosVertexZSpecifierGapWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent ): ReosVertexZSpecifierDependentOtherVertexWidget( map, domain, parent )
{
  init();
}

QIcon ReosVertexZSpecifierGapWidget::icon() const
{
  return QIcon( QPixmap( "://toolbar/ZSpecifierVertexAndGap.png" ) );
}

ReosVertexZSpecifierDependOnOtherVertexFactory &ReosVertexZSpecifierGapWidget::dependentVertexFactory() {return mFactory;}

ReosFormParameterSimpleDouble *ReosVertexZSpecifierGapWidget::makeValueParameterForm( ReosForm *parentForm )
{
  return new ReosFormParameterSimpleDouble( tr( "Gap : " ), 0, parentForm, " m" );
}

void ReosVertexZSpecifierGapWidget::setValueInFactory( double value )
{
  mFactory.setGap( value );
}

ReosVertexZSpecifierEntryWidgetModel::ReosVertexZSpecifierEntryWidgetModel( QList<ReosVertexZSpecifierEntryWidget *> &entriesList, QObject *parent ): QAbstractListModel( parent ),
  mEntriesList( entriesList )
{

}

int ReosVertexZSpecifierEntryWidgetModel::rowCount( const QModelIndex &parent ) const
{
  Q_UNUSED( parent );
  return mEntriesList.count();

}

QVariant ReosVertexZSpecifierEntryWidgetModel::data( const QModelIndex &index, int role ) const
{
  if ( role == Qt::DecorationRole )
    return mEntriesList.at( index.row() )->icon();

  return QVariant();
}
