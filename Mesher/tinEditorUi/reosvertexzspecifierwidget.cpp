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

QString vertexReferenceText( VertexPointer vert )
{
  QString str;
  if ( vert == nullptr )
  {
    str = QObject::tr( "no reference" ).prepend( "<b><font color=\"red\">" ).append( "</font></b>" );
  }
  else
  {
    str = QString( "<font color=\"green\"><b>X= %1 , Y= %2</font></b>" ).
          arg( QString::number( vert->x(), 'f', 2 ) ).
          arg( QString::number( vert->y(), 'f', 2 ) ) ;
  }

  return str;
}

ReosVertexZSpecifierWidget::ReosVertexZSpecifierWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosVertexZSpecifierWidget )
{
  ui->setupUi( this );
  addEntry( new ReosVertexZSpecifierSimpleValueWidget( this ) );
  addEntry( new ReosVertexZSpecifierSlopeWidget( map, domain, this ) );
  addEntry( new ReosVertexZSpecifierGapWidget( map, domain, this ) );
  addEntry( new ReosVertexZSpecifierInterpolationWidget( map, domain, this ) );

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
    mCurrentEntryWidget->assignZSpecifier( vert );
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
  if ( mCurrentEntryWidget )
    mCurrentEntryWidget->stop();
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

void ReosVertexZSpecifierEntryWidget::assignZSpecifier( VertexPointer vert )
{
  vert->setZSpecifier( factory() );
}

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
  new ReosFormText( referenceForm, tr( "Reference coordinates : " ) );
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
    startSelectReference();
  else
    static_cast<ReosMeshItemVertex *>( dependentVertexFactory().otherVertex()->graphicPointer() )->setReference( true );


}

void ReosVertexZSpecifierDependentOtherVertexWidget::stop()
{
  hide();
  if ( mMap->getMaptool() == selectReferenceMapTool )
    selectReferenceMapTool->returnToPreviousMapTool();
  hideVertexReference();
}

void ReosVertexZSpecifierDependentOtherVertexWidget::vertexHasToBeRemoved( VertexPointer vertex )
{
  if ( vertex == dependentVertexFactory().otherVertex() )
  {
    clearVertexReference();
  }
}

void ReosVertexZSpecifierDependentOtherVertexWidget::startSelectReference()
{
  selectReferenceMapTool->start();
}

void ReosVertexZSpecifierDependentOtherVertexWidget::updateReferenceText()
{
  mReferenceText->setText( vertexReferenceText( dependentVertexFactory().otherVertex() ) );
}

void ReosVertexZSpecifierDependentOtherVertexWidget::zoneHasBeenSelected( const QRectF &rect )
{
  auto vert = mDomain->vertex( rect );
  if ( vert )
  {
    setVertexReference( vert );
    updateReferenceText();
    selectReferenceMapTool->returnToPreviousMapTool();
  }
}

void ReosVertexZSpecifierDependentOtherVertexWidget::valueHasBeenEdited()
{
  setValueInFactory( mValueParameterForm->getValue() );
}

void ReosVertexZSpecifierDependentOtherVertexWidget::setVertexReference( ReosMeshItemVertex *vert )
{
  clearVertexReference();
  dependentVertexFactory().setOtherVertex( vert->realWorldVertex() );
  vert->setReference( true );
}

void ReosVertexZSpecifierDependentOtherVertexWidget::clearVertexReference()
{
  hideVertexReference();
  dependentVertexFactory().setOtherVertex( nullptr );
}

void ReosVertexZSpecifierDependentOtherVertexWidget::hideVertexReference()
{
  if ( dependentVertexFactory().otherVertex() )
    static_cast<ReosMeshItemVertex *>( dependentVertexFactory().otherVertex()->graphicPointer() )->setReference( false );
}

void ReosVertexZSpecifierDependentOtherVertexWidget::assignZSpecifier( VertexPointer vert )
{
  vert->setZSpecifier( factory() );
  if ( mTakeNewVertexAsReference->getValue() )
  {
    if ( vert->graphicPointer() )
      setVertexReference( static_cast<ReosMeshItemVertex *>( vert->graphicPointer() ) );
  }

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

void ReosVertexZSpecifierSlopeWidget::clear()
{
  ReosVertexZSpecifierDependentOtherVertexWidget::clear();
  mFactory.setSlope( 0 );
}

void ReosVertexZSpecifierSlopeWidget::setSpecifier( ReosVertexZSpecifier *specifier )
{
  if ( specifier->type() != type() )
    return;

  auto slopeSpecifier = static_cast<ReosVertexZSpecifierOtherVertexAndSlope *>( specifier );

  mFactory.setSlope( slopeSpecifier->slope() );
  setValue( slopeSpecifier->slope() * 100 );
  setOtherVertex( slopeSpecifier->otherVertex() );
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

void ReosVertexZSpecifierGapWidget::clear()
{
  ReosVertexZSpecifierDependentOtherVertexWidget::clear();
  mFactory.setGap( 0 );
}

void ReosVertexZSpecifierGapWidget::setSpecifier( ReosVertexZSpecifier *specifier )
{
  if ( specifier->type() != type() )
    return;

  auto gapSpecifier = static_cast<ReosVertexZSpecifierOtherVertexAndGap *>( specifier );

  mFactory.setGap( gapSpecifier->gap() );
  setValue( gapSpecifier->gap() );
  setOtherVertex( gapSpecifier->otherVertex() );
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

  if ( role == Qt::DisplayRole && mTextDisplayed )
    return mEntriesList.at( index.row() )->description();


  return QVariant();
}



ReosVertexZSpecifierInterpolationWidget::ReosVertexZSpecifierInterpolationWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent ):
  ReosVertexZSpecifierEntryWidget( parent ), mMap( map ), mDomain( domain )
{
  mInterpolationLine = new ReosMapItemPolyline( map->getMapCanvas(), QPolygonF() );
  mInterpolationLine->hide();

  QPen interpolationLinePen( Qt::gray );
  interpolationLinePen.setWidth( 3 );
  interpolationLinePen.setStyle( Qt::DashDotLine );
  mInterpolationLine->setPen( interpolationLinePen );
  mInterpolationLine->setZValue( domain->zValue() + 5 );

  mForm = new ReosForm( this );
  mForm->setOrientation( Qt::Horizontal );

  mSelectReferenceAction = new ReosFormAction( mForm,
      QIcon( QPixmap( "://toolbar/ZSpecifierSelectDependingVertex.png" ) ),
      tr( "Select extremity vertices" ) );
  mSelectReferenceAction->setCheckable( true );
  auto referencesForm = new ReosForm( mForm );
  referencesForm->setOrientation( Qt::Vertical );

  auto firstReferenceForm = new ReosForm( referencesForm );
  firstReferenceForm->setOrientation( Qt::Horizontal );
  new ReosFormText( firstReferenceForm, tr( "First extremity : " ) );
  mFirstReferenceText = new ReosFormText( firstReferenceForm, "" );
  referencesForm->addForm( firstReferenceForm );

  auto secondReferenceForm = new ReosForm( referencesForm );
  secondReferenceForm->setOrientation( Qt::Horizontal );
  new ReosFormText( secondReferenceForm, tr( "Second extremity : " ) );
  mSecondReferenceText = new ReosFormText( secondReferenceForm, "" );
  referencesForm->addForm( secondReferenceForm );

  mForm->addForm( referencesForm );

  setLayout( new QHBoxLayout );
  layout()->addWidget( mForm->getWidget() );

  selectReferenceMapTool = new ReosVertexZSpecifierSelectReferenceMapTool( mMap, mDomain );
  selectReferenceMapTool->setAction( mSelectReferenceAction->action() );

  connect( mSelectReferenceAction->action(), &QAction::triggered, this, &ReosVertexZSpecifierInterpolationWidget::startSelectReferences );
  connect( selectReferenceMapTool, &ReosMapToolSelection::zonalCanvasRect, this, &ReosVertexZSpecifierInterpolationWidget::zoneHasBeenSelected );
}

QIcon ReosVertexZSpecifierInterpolationWidget::icon() const
{
  return QIcon( QPixmap( "://toolbar/ZSpecifierInterpolator.png" ) );
}

void ReosVertexZSpecifierInterpolationWidget::clear()
{
  clearExtremityReferences();
}

void ReosVertexZSpecifierInterpolationWidget::setSpecifier( ReosVertexZSpecifier *specifier )
{
  mFactory.setIntendedVertex( specifier->associatedVertex() );

  if ( specifier->type() != type() )
    return;

  mFactory.setInterpolatedVertex( specifier->associatedVertex() );

  updateInterpolationLine();
  updateReferencesText();
  showExtremityReferences();
}

ReosVertexZSpecifierFactory &ReosVertexZSpecifierInterpolationWidget::factory()
{
  return mFactory;
}

void ReosVertexZSpecifierInterpolationWidget::start()
{
  mInterpolationLine->show();
  updateReferencesText();
  show();
  if ( !mFactory.firstExtremity() || !mFactory.secondExtremity() )
    startSelectReferences();
  showExtremityReferences();
}

void ReosVertexZSpecifierInterpolationWidget::stop()
{
  mInterpolationLine->hide();
  hide();
  if ( mMap->getMaptool() == selectReferenceMapTool )
    selectReferenceMapTool->returnToPreviousMapTool();
  hideExtremityReferences();
}

void ReosVertexZSpecifierInterpolationWidget::assignZSpecifier( VertexPointer vert )
{
  ReosVertexZSpecifierEntryWidget::assignZSpecifier( vert );
  updateInterpolationLine();
}

void ReosVertexZSpecifierInterpolationWidget::vertexHasToBeRemoved( VertexPointer vertex )
{
  if ( vertex == mFactory.firstExtremity() || vertex == mFactory.secondExtremity() )
  {
    clearExtremityReferences();
  }
}

void ReosVertexZSpecifierInterpolationWidget::updateReferencesText()
{
  mFirstReferenceText->setText( vertexReferenceText( mFactory.firstExtremity() ) );
  mSecondReferenceText->setText( vertexReferenceText( mFactory.secondExtremity() ) );
}

void ReosVertexZSpecifierInterpolationWidget::startSelectReferences()
{
  clearExtremityReferences();
  updateInterpolationLine();
  mInterpolationLine->hide();
  updateReferencesText();
  selectReferenceMapTool->start();
}

void ReosVertexZSpecifierInterpolationWidget::zoneHasBeenSelected( const QRectF &zone )
{
  auto vert = mDomain->vertex( zone );
  if ( vert )
  {
    hideExtremityReferences();

    if ( mFactory.firstExtremity() )
    {
      mFactory.setExtremitiesVertices( mFactory.firstExtremity(), vert->realWorldVertex() );
      selectReferenceMapTool->returnToPreviousMapTool();
      updateInterpolationLine();
      mInterpolationLine->show();
    }
    else
      mFactory.setExtremitiesVertices( vert->realWorldVertex(), nullptr );

    updateReferencesText();
    showExtremityReferences();
  }
}

void ReosVertexZSpecifierInterpolationWidget::updateInterpolationLine()
{
  const std::list<VertexPointer> &addedVertex = mFactory.interpolatedVertices();
  QPolygonF points;
  if ( mFactory.firstExtremity() && mFactory.secondExtremity() )
  {
    points.append( QPointF( mFactory.firstExtremity()->x(), mFactory.firstExtremity()->y() ) );
    for ( auto v : addedVertex )
    {
      points.append( QPointF( v->x(), v->y() ) );
    }
    points.append( QPointF( mFactory.secondExtremity()->x(), mFactory.secondExtremity()->y() ) );
  }

  mInterpolationLine->setPolyline( points );
}

void ReosVertexZSpecifierInterpolationWidget::showExtremityReferences()
{
  if ( mFactory.firstExtremity() )
    static_cast<ReosMeshItemVertex *>( mFactory.firstExtremity()->graphicPointer() )->setReference( true );
  if ( mFactory.secondExtremity() )
    static_cast<ReosMeshItemVertex *>( mFactory.secondExtremity()->graphicPointer() )->setReference( true );
}

void ReosVertexZSpecifierInterpolationWidget::clearExtremityReferences()
{
  hideExtremityReferences();
  mFactory.setExtremitiesVertices( nullptr, nullptr );
}

void ReosVertexZSpecifierInterpolationWidget::hideExtremityReferences()
{
  if ( mFactory.firstExtremity() )
    static_cast<ReosMeshItemVertex *>( mFactory.firstExtremity()->graphicPointer() )->setReference( false );
  if ( mFactory.secondExtremity() )
    static_cast<ReosMeshItemVertex *>( mFactory.secondExtremity()->graphicPointer() )->setReference( false );
}


