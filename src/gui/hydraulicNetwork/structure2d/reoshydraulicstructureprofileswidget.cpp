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
#include "reosstyleregistery.h"

ReosHydraulicStructureProfilesWidget::ReosHydraulicStructureProfilesWidget( ReosHydraulicStructure2D *structure, const ReosGuiContext &guiContext )
  : ReosStackedPageWidget( guiContext.parent() )
  , ui( new Ui::ReosHydraulicStructureProfilesWidget )
  , mGuiContext( guiContext )
  , mStructure( structure )
  , mMapStructureItem( guiContext.map(), structure->geometryStructure() )
  , mActionAddProfile( new QAction( QIcon( QStringLiteral( ":/images/add.svg" ) ), tr( "Add a New Profile" ), this ) )
  , mMapToolAddProfile( new ReosMapToolDrawPolyline( this, guiContext.map() ) )
  , mActionSelectProfile( new QAction( QIcon( QStringLiteral( ":/images/neutral.svg" ) ), tr( "Select a Profile" ), this ) )
  , mMapToolSelectProfile( new ReosMapToolSelectMapItem( guiContext.map(),  QStringLiteral( "hydraulic-structure-profile" ) ) )
  , mActionEditProfile( new QAction( QIcon( QStringLiteral( ":/images/editProfile.svg" ) ), tr( "Edit Current Profile on Map" ), this ) )
  , mMapToolEditProfile( new ReosMapToolEditMapPolyline( this, guiContext.map() ) )
  , mActionRemoveProfile( new QAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Current Profile" ), this ) )
  , mActionRenameProfile( new QAction( QIcon( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Current Profile" ), this ) )
  , mActionDisplayVelocity( new QAction( tr( "Display Velocity" ), this ) )
{
  ui->setupUi( this );
  setObjectName( QStringLiteral( "hydraulic-structure-orofile-widget" ) );

  QString settingsString = QStringLiteral( "hydraulic-structure-profile-widget" );
  ui->mPlotWidget->setSettingsContext( settingsString );
  ReosSettings settings;

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

  toolBar->addAction( mActionSelectProfile );
  mActionSelectProfile->setCheckable( true );
  mMapToolSelectProfile->setAction( mActionSelectProfile );
  mMapToolSelectProfile->setCursor( Qt::ArrowCursor );
  mMapToolSelectProfile->setSearchItemWhenMoving( true );
  connect( mMapToolSelectProfile, &ReosMapToolSelectMapItem::found, this, &ReosHydraulicStructureProfilesWidget::onProfileSelected );

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
  mTerrainProfileCurve->setAutoScale( false );
  ui->mPlotWidget->addPlotItem( mTerrainProfileCurve );

  mWaterLevelProfileCurve = new ReosPlotCurve( tr( "Water Level" ), Qt::blue, 2 );
  mWaterLevelProfileCurve->setAutoScale( false );
  ui->mPlotWidget->addPlotItem( mWaterLevelProfileCurve );

  mVelocityProfileCurve = new ReosPlotCurve( tr( "Velocity" ), Qt::darkGreen, 2 );
  mVelocityProfileCurve->setOnRightAxe();
  mVelocityProfileCurve->setAutoScale( false );
  ui->mPlotWidget->addPlotItem( mVelocityProfileCurve );

  QBrush brush( Qt::SolidPattern );
  brush.setColor( QColor( 0, 100, 250, 150 ) );
  mFilledWater = new ReosPlotPolygons();
  mFilledWater->setBrush( brush );
  QPen pen;
  pen.setStyle( Qt::NoPen );
  mFilledWater->setPen( pen );
  mFilledWater->setAutoScale( false );
  ui->mPlotWidget->addPlotItem( mFilledWater );
  ui->mPlotWidget->setTitleAxeYRight( tr( "Velocity" ) );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Elevation" ) );
  ui->mPlotWidget->setTitleAxeX( tr( "Distance" ) );

  mActionDisplayVelocity->setCheckable( true );
  if ( settings.contains( settingsString + QStringLiteral( "/display-velocity" ) ) )
    mActionDisplayVelocity->setChecked( settings.value( settingsString + QStringLiteral( "/display-velocity" ) ).toBool() );
  else
    mActionDisplayVelocity->setChecked( true );

  ui->mPlotWidget->enableAxeYRight( mActionDisplayVelocity->isChecked() );
  mVelocityProfileCurve->setVisible( mActionDisplayVelocity->isChecked() );
  mVelocityProfileCurve->setLegendActive( mActionDisplayVelocity->isChecked(), true );

  QList<QAction *> plotActions;
  plotActions << mActionDisplayVelocity;
  ui->mPlotWidget->addActions( plotActions );

  connect( mActionDisplayVelocity, &QAction::triggered, this, [this, settingsString]
  {
    ui->mPlotWidget->enableAxeYRight( mActionDisplayVelocity->isChecked() );
    mVelocityProfileCurve->setVisible( mActionDisplayVelocity->isChecked() );
    mVelocityProfileCurve->setLegendActive( mActionDisplayVelocity->isChecked(), true );
    ReosSettings settings;
    settings.setValue( settingsString + QStringLiteral( "/display-velocity" ), mActionDisplayVelocity->isChecked() );
  } );

  syncProfiles();
  onCurrentProfileChanged();

  connect( guiContext.map(), &ReosMap::timeChanged, this, &ReosHydraulicStructureProfilesWidget::onTimeChanged );
  connect( ui->mPlotWidget, &ReosPlotWidget::cursorMoved, this, &ReosHydraulicStructureProfilesWidget::onPlotCursorMove );

  connect( ui->mDetachButton, &QPushButton::clicked, this, [this]
  {
    detach( mGuiContext.parent() );
  } );
}


ReosHydraulicStructureProfilesWidget::~ReosHydraulicStructureProfilesWidget()
{
  delete ui;
}

void ReosHydraulicStructureProfilesWidget::showBackButton()
{
  ui->mBackButton->show();
}

void ReosHydraulicStructureProfilesWidget::hideBackButton()
{
  ui->mBackButton->hide();
}

void ReosHydraulicStructureProfilesWidget::hideDetachButton()
{
  ui->mDetachButton->hide();
}

void ReosHydraulicStructureProfilesWidget::showEvent( QShowEvent *e )
{
  if ( mStructure->profilesCount() == 0 )
    mMapToolAddProfile->setCurrentToolInMap();
  else
    mMapToolSelectProfile->setCurrentToolInMap();

  QWidget::showEvent( e );
}

void ReosHydraulicStructureProfilesWidget::hideEvent( QHideEvent *e )
{
  mGuiContext.map()->setDefaultMapTool();

  QWidget::hideEvent( e );
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
    mVelocityProfileCurve->setData( mCurrentProfile->resultsProfile( mStructure->network()->currentScheme(),
                                    time,
                                    ReosHydraulicSimulationResults::DatasetType::Velocity ) );
    QPolygonF waterSurface;
    mFilledWater->setPolygons( mCurrentProfile->resultsFilledByWater( mStructure->network()->currentScheme(),
                               time,
                               waterSurface ) );
    mWaterLevelProfileCurve->setData( waterSurface );
  }
}

void ReosHydraulicStructureProfilesWidget::onPlotCursorMove( const QPointF &pos )
{
  auto it = mMapProfiles.find( mCurrentProfile );
  if ( it != mMapProfiles.end() )
    it.value()->setMarkerDistance( pos.x() );
}

void ReosHydraulicStructureProfilesWidget::onProfileSelected( ReosMapItem *item, const QPointF & )
{
  for ( auto it = mMapProfiles.constBegin(); it != mMapProfiles.constEnd(); ++it )
  {
    if ( it.value().get() == item )
    {
      ui->mProfileComboBox->setCurrentIndex( mStructure->profileIndex( it.key() ) );
      break;
    }
  }
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
  mapProf->setDescription( QStringLiteral( "hydraulic-structure-profile" ) );
  mapProf->setColor( ReosStyleRegistery::instance()->greenReos() );
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
    mp->setColor( ReosStyleRegistery::instance()->redReos() );
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
    mp->setColor( ReosStyleRegistery::instance()->greenReos() );
    mMapToolEditProfile->setMapPolyline( nullptr );
    mp->setMarkerAtMid();
    mp->setZValue( mp->ZValue() - 1 );
  }
}

void ReosHydraulicStructureProfilesWidget::updateCurrentProfileValues()
{
  if ( mCurrentProfile )
  {
    ReosHydraulicScheme *scheme = mStructure->network()->currentScheme();
    mTerrainProfileCurve->setData( mCurrentProfile->terrainProfile() );

    if ( mCurrentProfile->hasResults( scheme ) )
    {
      mVelocityProfileCurve->setData( mCurrentProfile->resultsProfile( scheme,
                                      mGuiContext.map()->currentTime(),
                                      ReosHydraulicSimulationResults::DatasetType::Velocity ) );

      QPolygonF waterSurface;
      mFilledWater->setPolygons( mCurrentProfile->resultsFilledByWater( scheme,
                                 mGuiContext.map()->currentTime(),
                                 waterSurface ) );
      mWaterLevelProfileCurve->setData( waterSurface );
    }
    else
    {
      mVelocityProfileCurve->setData( QPolygonF() );
      mWaterLevelProfileCurve->setData( QPolygonF() );
    }


    if ( mCurrentProfile )
    {
      ui->mPlotWidget->setExtent( mCurrentProfile->elevationExtent( scheme ) );
      if ( mCurrentProfile->hasResults( scheme ) )
      {
        QPair<double, double> rightExtent = mCurrentProfile->valueVerticalExtent( scheme, ReosHydraulicSimulationResults::DatasetType::Velocity );
        ui->mPlotWidget->setAxeYRightExtent( rightExtent.first, rightExtent.second );
      }
      ui->mPlotWidget->updatePlot();
    }
  }
  else
  {
    mTerrainProfileCurve->setData( QPolygonF() );
    mWaterLevelProfileCurve->setData( QPolygonF() );
    mFilledWater->setPolygons( QList<QPolygonF>() );
  }
}
