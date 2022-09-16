/***************************************************************************
  reoshydraulicstructureprofileswidget.cpp - ReosHydraulicStructureProfilesWidget

 ---------------------
 begin                : 28.8.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshydraulicstructureprofileswidget.h"
#include "ui_reoshydraulicstructureprofileswidget.h"

#include <QMessageBox>

#include "reosguicontext.h"
#include "reosmaptool.h"
#include "reoshydraulicstructure2d.h"
#include "reosformwidget.h"

ReosHydraulicStructureProfilesWidget::ReosHydraulicStructureProfilesWidget( ReosHydraulicStructure2D *structure, const ReosGuiContext &guiContext )
  : ReosStackedPageWidget( guiContext.parent() )
  , ui( new Ui::ReosHydraulicStructureProfilesWidget )
  , mGuiContext( guiContext )
  , mStructure( structure )
  , mMapStructureItem( guiContext.map(), structure->geometryStructure() )
  , mActionAddProfile( new QAction( QPixmap( QStringLiteral( ":/images/add.svg" ) ), tr( "Add a New Profile" ), this ) )
  , mMapToolAddProfile( new ReosMapToolDrawPolyline( this, guiContext.map() ) )
  , mActionEditProfile( new QAction( QPixmap( QStringLiteral( ":/images/editProfile.svg" ) ), tr( "Edit Current Profile on Map" ), this ) )
  , mMapToolEditProfile( new ReosMapToolEditMapPolyline( this, guiContext.map() ) )
  , mActionRemoveProfile( new QAction( QPixmap( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Current Profile" ), this ) )
  , mActionRenameProfile( new QAction( QPixmap( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Current Profile" ), this ) )
{
  ui->setupUi( this );

  ui->mProfileComboBox->setModel( mStructure->profilesCollection() );
  connect( ui->mProfileComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHydraulicStructureProfilesWidget::onCurrentProfileChanged );

  mMapStructureItem.setLineWidth( 2 );

  QToolBar *toolBar = new QToolBar( this );

  toolBar->addAction( mActionAddProfile );
  mActionAddProfile->setCheckable( true );
  mMapToolAddProfile->setAction( mActionAddProfile );
  mMapToolAddProfile->setColor( Qt::black );
  mMapToolAddProfile->setSecondaryStrokeColor( Qt::white );
  mMapToolAddProfile->setLineStyle( Qt::DashLine );
  mMapToolAddProfile->setAllowSelfIntersect( false );
  mMapToolAddProfile->setStrokeWidth( 3 );
  mMapToolAddProfile->enableSnapping( true );
  mMapToolAddProfile->setAllowSelfIntersect( true );
  connect( mMapToolAddProfile, &ReosMapToolDrawPolyline::drawn, this, &ReosHydraulicStructureProfilesWidget::onNewProfileAdded );

  toolBar->addAction( mActionEditProfile );
  mActionEditProfile->setCheckable( true );
  mMapToolEditProfile->setAction( mActionEditProfile );
  toolBar->addAction( mActionRemoveProfile );
  toolBar->addAction( mActionRenameProfile );
  ui->mToolBarLayout->addWidget( toolBar );
  connect( mMapToolEditProfile, &ReosMapToolEditMapPolyline::polylineEdited, this, &ReosHydraulicStructureProfilesWidget::onCurrentProfileEdited );

  connect( mActionRemoveProfile, &QAction::triggered, this, &ReosHydraulicStructureProfilesWidget::onRemoveProfile );
  connect( mActionRenameProfile, &QAction::triggered, this, &ReosHydraulicStructureProfilesWidget::onRenameProfile );
  connect( ui->mBackButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );

  mTerrainProfileCurve = new ReosPlotCurve( tr( "Terrain" ), Qt::black, 2 );
  ui->mPlotWidget->addPlotItem( mTerrainProfileCurve );

  mWaterLevelProfileCurve = new ReosPlotCurve( tr( "Water Level" ), Qt::blue, 2 );
  ui->mPlotWidget->addPlotItem( mWaterLevelProfileCurve );

  mVelocityProfileCurve = new ReosPlotCurve( tr( "Velocity" ), Qt::darkGreen, 2 );
  mVelocityProfileCurve->setOnRightAxe();
  ui->mPlotWidget->addPlotItem( mVelocityProfileCurve );

  ui->mPlotWidget->enableAxeYRight( true );
  ui->mPlotWidget->setTitleAxeYRight( tr( "Velocity" ) );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Elevation" ) );
  ui->mPlotWidget->setTitleAxeX( tr( "Distance" ) );

  syncProfiles();
  onCurrentProfileChanged();

  connect( guiContext.map(), &ReosMap::timeChanged, this, &ReosHydraulicStructureProfilesWidget::onTimeChanged );
  connect( ui->mPlotWidget, &ReosPlotWidget::cursorMoved, this, &ReosHydraulicStructureProfilesWidget::onPlotCursorMove );
}


ReosHydraulicStructureProfilesWidget::~ReosHydraulicStructureProfilesWidget()
{
  delete ui;
}

void ReosHydraulicStructureProfilesWidget::onNewProfileAdded( const QPolygonF &profile )
{
  ReosParameterString name( tr( "Profile name" ), false );

  name.setValue( tr( "Profile %1" ).arg( mStructure->profilesCount() + 1 ) );

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->addText( tr( "Enter a name for the profile" ) );
  dial->addParameter( &name );
  dial->exec();

  int profileIndex = mStructure->createProfile( name.value(), profile, mMapToolAddProfile->crs() );

  createMapProfile( profileIndex, profile );

  ui->mProfileComboBox->setCurrentIndex( profileIndex );
  onCurrentProfileChanged();
}

void ReosHydraulicStructureProfilesWidget::onCurrentProfileChanged()
{
  unselectProfile( mCurrentProfile );
  mCurrentProfile = mStructure->profile( ui->mProfileComboBox->currentIndex() );
  selectProfile( mCurrentProfile );

  updateCurrentProfileValues();

  mTerrainProfileCurve->zoomOnExtent();

}

void ReosHydraulicStructureProfilesWidget::onRemoveProfile()
{
  int currentIndex = ui->mProfileComboBox->currentIndex();
  ReosHydraulicStructureProfile *profile = mStructure->profile( currentIndex );
  if ( !profile )
    return;
  if ( QMessageBox::warning( this, tr( "Remove Profile" ),
                             tr( "Do yo want to remove the current profile \"%1\"?" ).arg( profile->name() ),
                             QMessageBox::Yes | QMessageBox::No,
                             QMessageBox::No ) == QMessageBox::No )
    return;

  removeMapProfile( profile );
  mStructure->removeProfile( currentIndex );
  onCurrentProfileChanged();
}

void ReosHydraulicStructureProfilesWidget::onRenameProfile()
{
  int currentIndex = ui->mProfileComboBox->currentIndex();
  ReosHydraulicStructureProfile *profile = mStructure->profile( currentIndex );
  if ( !profile )
    return;

  ReosFormDialog *diag = new ReosFormDialog( this );

  diag->addText( "Enter the new name:" );

  ReosParameterString name( tr( "Profile name:" ), false );
  name.setValue( profile->name() );
  diag->addParameter( &name );

  if ( diag->exec() )
    mStructure->renameProfile( currentIndex, name.value() );

}

void ReosHydraulicStructureProfilesWidget::onTimeChanged( const QDateTime &time )
{
  if ( mCurrentProfile )
  {
    mWaterLevelProfileCurve->setData( mCurrentProfile->resultsProfile( mStructure->network()->currentScheme(),
                                      time,
                                      ReosHydraulicSimulationResults::DatasetType::WaterLevel ) );
    mVelocityProfileCurve->setData( mCurrentProfile->resultsProfile( mStructure->network()->currentScheme(),
                                    time,
                                    ReosHydraulicSimulationResults::DatasetType::Velocity ) );
  }
}

void ReosHydraulicStructureProfilesWidget::onPlotCursorMove( const QPointF &pos )
{
  auto it = mMapProfiles.find( mCurrentProfile );
  if ( it != mMapProfiles.end() )
    it.value()->setMarkerDistance( pos.x() );
}

void ReosHydraulicStructureProfilesWidget::onCurrentProfileEdited()
{
  if ( mCurrentProfile )
  {
    QPolygonF geom;
    auto it = mMapProfiles.find( mCurrentProfile );
    if ( it != mMapProfiles.end() )
      geom = it.value()->mapPolyline();
    mCurrentProfile->changeGeometry( geom, mGuiContext.map()->mapCrs() );
  }

  updateCurrentProfileValues();

  mTerrainProfileCurve->zoomOnExtent();
}

void ReosHydraulicStructureProfilesWidget::syncProfiles()
{
  mMapProfiles.clear();
  int profileCount = mStructure->profilesCount();
  for ( int i = 0; i < profileCount; ++i )
    createMapProfile( i, mStructure->profile( i )->geometry() );
}

void ReosHydraulicStructureProfilesWidget::createMapProfile( int profileIndex, const QPolygonF &profile )
{
  MapProfile mapProf = std::make_shared<ReosMapPolyline>( mGuiContext.map(), profile );
  mapProf->setColor( Qt::black );
  mapProf->setExternalColor( Qt::white );
  mapProf->setWidth( 3 );
  mapProf->setExternalWidth( 5 );
  mapProf->setMarkerArrow( true );
  mapProf->setMarkerAtMid();
  mMapProfiles.insert( mStructure->profile( profileIndex ), mapProf );
}

void ReosHydraulicStructureProfilesWidget::removeMapProfile( ReosHydraulicStructureProfile *profile )
{
  if ( profile == mCurrentProfile )
    unselectProfile( profile );
  mMapProfiles.remove( profile );
}

void ReosHydraulicStructureProfilesWidget::selectProfile( ReosHydraulicStructureProfile *profile )
{
  MapProfile mp = mMapProfiles.value( profile );
  if ( mp )
  {
    mp->activeMarker( true );
    mp->setColor( Qt::red );
    mMapToolEditProfile->setMapPolyline( mp.get() );
    mp->setZValue( mp->ZValue() + 1 );
  }
}

void ReosHydraulicStructureProfilesWidget::unselectProfile( ReosHydraulicStructureProfile *profile )
{
  MapProfile mp = mMapProfiles.value( profile );
  if ( mp )
  {
    mp->activeMarker( false );
    mp->setColor( Qt::black );
    mMapToolEditProfile->setMapPolyline( nullptr );
    mp->setMarkerAtMid();
    mp->setZValue( mp->ZValue() - 1 );
  }
}

void ReosHydraulicStructureProfilesWidget::updateCurrentProfileValues()
{
  if ( mCurrentProfile )
  {
    mTerrainProfileCurve->setData( mCurrentProfile->terrainProfile() );
    mWaterLevelProfileCurve->setData(
      mCurrentProfile->resultsProfile( mStructure->network()->currentScheme(),
                                       mGuiContext.map()->currentTime(),
                                       ReosHydraulicSimulationResults::DatasetType::WaterLevel ) );
    mVelocityProfileCurve->setData( mCurrentProfile->resultsProfile( mStructure->network()->currentScheme(),
                                    mGuiContext.map()->currentTime(),
                                    ReosHydraulicSimulationResults::DatasetType::Velocity ) );
  }
  else
  {
    mTerrainProfileCurve->setData( QPolygonF() );
    mWaterLevelProfileCurve->setData( QPolygonF() );
  }
}
