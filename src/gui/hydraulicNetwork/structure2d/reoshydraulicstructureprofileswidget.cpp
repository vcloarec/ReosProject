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

#include "reosguicontext.h"
#include "reosmaptool.h"
#include "reosstyleregistery.h"
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
  connect( mMapToolAddProfile, &ReosMapToolDrawPolyline::drawn, this, &ReosHydraulicStructureProfilesWidget::onNewProfileAdded );

  toolBar->addAction( mActionEditProfile );
  mActionEditProfile->setCheckable( true );
  mMapToolEditProfile->setAction( mActionEditProfile );
  toolBar->addAction( mActionRemoveProfile );
  toolBar->addAction( mActionRenameProfile );
  ui->mToolBarLayout->addWidget( toolBar );

  connect( ui->mBackButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );
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

  mMapProfiles.insert( mStructure->profile( profileIndex ), createMapProfile( profile ) );

  ui->mProfileComboBox->setCurrentIndex( profileIndex );
  onCurrentProfileChanged();
}

void ReosHydraulicStructureProfilesWidget::onCurrentProfileChanged()
{
  MapProfile mp = mMapProfiles.value( mStructure->profile( ui->mProfileComboBox->currentIndex() ) );
  if ( mp )
  {
    mp->setColor( Qt::red );
    mMapToolEditProfile->setMapPolyline( mp.get() );
  }
}

ReosHydraulicStructureProfilesWidget::MapProfile ReosHydraulicStructureProfilesWidget::createMapProfile( const QPolygonF &profile )
{
  MapProfile ret = std::make_shared<ReosMapPolyline>( mGuiContext.map(), profile );

  ret->setColor( Qt::black );
  ret->setExternalColor( Qt::white );
  ret->setWidth( 3 );
  ret->setExternalWidth( 5 );

  return ret;
}
