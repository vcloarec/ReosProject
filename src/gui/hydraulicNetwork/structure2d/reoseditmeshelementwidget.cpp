/***************************************************************************
  reoseditmeshelementwidget.cpp - ReosEditMeshElementWidget

 ---------------------
 begin                : 9.3.2022
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
#include "reoseditmeshelementwidget.h"
#include "ui_reoseditmeshelementwidget.h"

#include <qgsmapcanvas.h>
#include <qgsrubberband.h>
#include <qgsgeometryengine.h>

#include "reosmesh.h"
#include "reosmaptooleditmeshframe.h"
#include "reosstyleregistery.h"
#include "reossettings.h"
#include "reosapplication.h"
#include "reosprocesscontroler.h"

ReosEditMeshElementWidget::ReosEditMeshElementWidget( ReosMesh *mesh, const ReosGuiContext &context )
  : QWidget( context.parent() )
  , ui( new Ui::ReosEditMeshElementWidget )
  , mMesh( mesh )
  , mGuiContext( context, this )
  , mActionEditMeshFrame( new QAction( QPixmap( QStringLiteral( ":/images/editMeshFrameTool.svg" ) ), tr( "Edit Mesh Elements" ), this ) )
  , mMapToolEditMeshFrame( new ReosMapToolEditMeshFrame( mesh, this, mGuiContext.map() ) )
  , mDisplayTopograhy( new ReosParameterBoolean( tr( "Display topography" ), false, this ) )
{
  ui->setupUi( this );

  mToolBar = new QToolBar( this );
  mToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBarLayout->addWidget( mToolBar );

  mToolBar->addAction( mActionEditMeshFrame );
  mActionEditMeshFrame->setCheckable( true );
  mMapToolEditMeshFrame->setAction( mActionEditMeshFrame );

  mToolBar->addActions( mMapToolEditMeshFrame->mainActions()->actions() );
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "/edit-mesh-element/display-topography" ) ) )
    mDisplayTopograhy->setValue( settings.value( QStringLiteral( "/edit-mesh-element/display-topography" ) ).toBool() );
  else
    mDisplayTopograhy->setValue( false );

  ReosParameterBooleanWidget *checkBoxTopography = new ReosParameterBooleanWidget( this );
  checkBoxTopography->setBooleanParameter( mDisplayTopograhy );
  mToolBar->addWidget( checkBoxTopography );
  connect( mDisplayTopograhy, &ReosParameterBoolean::valueChanged, this, [this]
  {
    if ( mDisplayTopograhy->value() )
      mMesh->activateDataset( mMesh->verticesElevationDatasetId() );
    else
      mMesh->activateDataset( QString() );
    ReosSettings settings;
    settings.setValue( QStringLiteral( "/edit-mesh-element/display-topography" ), mDisplayTopograhy->value() );
  } );

  ui->mParameterMinimumAngle->setDouble( mesh->qualityMeshParameters().minimumAngle );
  ui->mParameterMinimumAngle->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->mParameterMaximumAngle->setDouble( mesh->qualityMeshParameters().maximumAngle );
  ui->mParameterMaximumAngle->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->mParameterConnectionCount->setInteger( mesh->qualityMeshParameters().connectionCount );
  ui->mParameterConnectionCount->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->mParameterConnectionBoundary->setInteger( mesh->qualityMeshParameters().connectionCountBoundary );
  ui->mParameterConnectionBoundary->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->mParameterMaximumSlope->setSlope( mesh->qualityMeshParameters().maximumSlope );
  ui->mParameterMaximumSlope->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->mParameterMinimumArea->setArea( mesh->qualityMeshParameters().minimumArea );
  ui->mParameterMinimumArea->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->mParameterMaximumArea->setArea( mesh->qualityMeshParameters().maximumArea );
  ui->mParameterMaximumArea->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  ui->mParameterMaximumAreaChange->setDouble( mesh->qualityMeshParameters().maximumAreaChange );
  ui->mParameterMaximumAreaChange->enableSpacer( ReosParameterAreaWidget::SpacerInMiddle );

  mParamWidgets << ui->mParameterMinimumAngle
                << ui->mParameterMaximumAngle
                << ui->mParameterConnectionCount
                << ui->mParameterConnectionBoundary
                << ui->mParameterMaximumSlope
                << ui->mParameterMinimumArea
                << ui->mParameterMaximumArea
                << ui->mParameterMaximumAreaChange;


  if ( settings.contains( QStringLiteral( "/edit-mesh-element/minimum-angle-color" ) ) )
    ui->mColorMinimumAngle->setColor( settings.value( QStringLiteral( "/edit-mesh-element/minimum-angle-color" ) ).value<QColor>() );
  else
    ui->mColorMinimumAngle->setColor( ReosStyleRegistery::instance()->fillColor() );

  if ( settings.contains( QStringLiteral( "/edit-mesh-element/maximum-angle-color" ) ) )
    ui->mColorMaximumAngle->setColor( settings.value( QStringLiteral( "/edit-mesh-element/maximum-angle-color" ) ).value<QColor>() );
  else
    ui->mColorMaximumAngle->setColor( ReosStyleRegistery::instance()->fillColor() );

  if ( settings.contains( QStringLiteral( "/edit-mesh-element/connection-count-color" ) ) )
    ui->mColorConnectionCount->setColor( settings.value( QStringLiteral( "/edit-mesh-element/connection-count-color" ) ).value<QColor>() );
  else
    ui->mColorConnectionCount->setColor( ReosStyleRegistery::instance()->fillColor() );

  if ( settings.contains( QStringLiteral( "/edit-mesh-element/connection-boundary-color" ) ) )
    ui->mColorConnectionBoundary->setColor( settings.value( QStringLiteral( "/edit-mesh-element/connection-boundary-color" ) ).value<QColor>() );
  else
    ui->mColorConnectionBoundary->setColor( ReosStyleRegistery::instance()->fillColor() );

  if ( settings.contains( QStringLiteral( "/edit-mesh-element/maximum-slope-color" ) ) )
    ui->mColorMaximumSlope->setColor( settings.value( QStringLiteral( "/edit-mesh-element/maximum-slope-color" ) ).value<QColor>() );
  else
    ui->mColorMaximumSlope->setColor( ReosStyleRegistery::instance()->fillColor() );

  if ( settings.contains( QStringLiteral( "/edit-mesh-element/minimum-area-color" ) ) )
    ui->mColorMinimumArea->setColor( settings.value( QStringLiteral( "/edit-mesh-element/minimum-area-color" ) ).value<QColor>() );
  else
    ui->mColorMinimumArea->setColor( ReosStyleRegistery::instance()->fillColor() );

  if ( settings.contains( QStringLiteral( "/edit-mesh-element/maximum-area-color" ) ) )
    ui->mColorMaximumArea->setColor( settings.value( QStringLiteral( "/edit-mesh-element/maximum-area-color" ) ).value<QColor>() );
  else
    ui->mColorMaximumArea->setColor( ReosStyleRegistery::instance()->fillColor() );

  if ( settings.contains( QStringLiteral( "/edit-mesh-element/maximum-area-change-color" ) ) )
    ui->mColorMaximumAreaChange->setColor( settings.value( QStringLiteral( "/edit-mesh-element/maximum-area-change-color" ) ).value<QColor>() );
  else
    ui->mColorMaximumAreaChange->setColor( ReosStyleRegistery::instance()->fillColor() );

  mColorButton << ui->mColorMinimumAngle
               << ui->mColorMaximumAngle
               << ui->mColorConnectionCount
               << ui->mColorConnectionBoundary
               << ui->mColorMaximumSlope
               << ui->mColorMinimumArea
               << ui->mColorMaximumArea
               << ui->mColorMaximumAreaChange;

  mCheckBoxes << ui->mCheckBoxMinimumAngle <<
              ui->mCheckBoxMaximumAngle <<
              ui->mCheckBoxConnectionCount <<
              ui->mCheckBoxConnectionBoundary <<
              ui->mCheckBoxMaximumSlope <<
              ui->mCheckBoxMinimumArea <<
              ui->mCheckBoxMaximumArea <<
              ui->mCheckBoxMaximumAreaChange;

  ui->mAutoUpdateCheckBox->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/auto-update-check" ) ).toBool() );
  ui->mCheckBoxMinimumAngle->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/minimum-angle-check" ) ).toBool() );
  ui->mCheckBoxMaximumAngle->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/maximum-angle-check" ) ).toBool() );
  ui->mCheckBoxConnectionCount->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/connection-check" ) ).toBool() );
  ui->mCheckBoxConnectionBoundary->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/connection-boundary-check" ) ).toBool() );
  ui->mCheckBoxMaximumSlope->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/maximum-slope-check" ) ).toBool() );
  ui->mCheckBoxMinimumArea->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/minimum-area-check" ) ).toBool() );
  ui->mCheckBoxMaximumArea->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/maximum-area-check" ) ).toBool() );
  ui->mCheckBoxMaximumAreaChange->setChecked( settings.value( QStringLiteral( "/edit-mesh-element/maximum-area-change-check" ) ).toBool() );

  connect( ui->mStartCheckButton, &QToolButton::clicked, this, &ReosEditMeshElementWidget::startCheckQualityControlled );
  connect( ui->mAutoUpdateCheckBox, &QCheckBox::stateChanged, this, &ReosEditMeshElementWidget::onQualityCheckBoxChanged );
  connect( mMesh, &ReosDataObject::dataChanged, this, &ReosEditMeshElementWidget::onMeshChanged );

  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mGuiContext.map()->mapCanvas() );
  if ( mapCanvas )
  {
    mMinimumAngleRubberBand = new QgsRubberBand( mapCanvas );
    mMaximumAngleRubberBand = new QgsRubberBand( mapCanvas );
    mConnectionCountBand = new QgsRubberBand( mapCanvas );
    mConnectionCountBand->setWidth( 6 );
    mConnectionBoundaryBand = new QgsRubberBand( mapCanvas );
    mConnectionBoundaryBand->setWidth( 6 );
    mMaximumSlopeBand = new QgsRubberBand( mapCanvas );
    mMaximumSlopeBand->setWidth( 4 );
    mMinimumAreaBand = new QgsRubberBand( mapCanvas );
    mMaximumAreaBand = new QgsRubberBand( mapCanvas );
    mMaximumAreaChangeBand = new QgsRubberBand( mapCanvas );

    mRubberBands << mMinimumAngleRubberBand
                 << mMaximumAngleRubberBand
                 << mConnectionCountBand
                 << mConnectionBoundaryBand
                 << mMaximumSlopeBand
                 << mMinimumAreaBand
                 << mMaximumAreaBand
                 << mMaximumAreaChangeBand;


    mOnMapButtons << ui->mOnMapButtonMinimumAngle <<
                  ui->mOnMapButtonMaximumAngle <<
                  ui->mOnMapButtonConnectionCount <<
                  ui->mOnMapButtonConnectionBoundary <<
                  ui->mOnMapButtonMaximumSlope <<
                  ui->mOnMapButtonMinimumArea <<
                  ui->mOnMapButtonMaximumArea <<
                  ui->mOnMapButtonMaximumAreaChange;

    for ( int i = 0; i < mRubberBands.count(); ++i )
    {
      QgsRubberBand *band = mRubberBands.at( i );
      connect( mOnMapButtons.at( i ), &QToolButton::clicked, this, [band, mapCanvas]
      {
        mapCanvas->setExtent( band->asGeometry().boundingBox() );
        mapCanvas->refresh();
      } );

      ReosColorButton *colorButton = mColorButton.at( i );
      QColor bandColor = colorButton->color();
      bandColor.setAlpha( 150 );
      band->setColor( bandColor );
      connect( colorButton, &ReosColorButton::colorChanged, this, [band, this]( const QColor & color )
      {
        onQualityCheckColorChanged();
        QColor bandColor( color );
        bandColor.setAlpha( 150 );
        band->setColor( bandColor );
        band->update();
      } );

      QCheckBox *cb = mCheckBoxes.at( i );
      band->setVisible( cb->isChecked() );
      QToolButton *mp = mOnMapButtons.at( i );
      connect( cb, &QCheckBox::stateChanged, this, [cb, band, mp, this]
      {
        onQualityCheckBoxChanged( cb->isChecked() );
        band->setVisible( cb->isChecked() );
        mp->setEnabled( cb->isChecked() );
      } );

      ReosParameterWidget *pw = mParamWidgets.at( i );
      connect( pw, &ReosParameterWidget::valueChanged, this, &ReosEditMeshElementWidget::onParameterValueChanged );
    }
  }

  for ( int i = 0; i < mRubberBands.count(); ++i )
    mRubberBands.at( i )->setVisible( ui->mQualityGroupBox->isChecked() && mCheckBoxes.at( i )->isChecked() );
  connect( ui->mQualityGroupBox, &QGroupBox::clicked, this, [this]
  {
    for ( int i = 0; i < mRubberBands.count(); ++i )
      mRubberBands.at( i )->setVisible( ui->mQualityGroupBox->isChecked() && mCheckBoxes.at( i )->isChecked() );
  } );

}

ReosEditMeshElementWidget::~ReosEditMeshElementWidget()
{
  if ( mMinimumAngleRubberBand )
    mMinimumAngleRubberBand->deleteLater();
  if ( mMaximumAngleRubberBand )
    mMaximumAngleRubberBand->deleteLater();
  if ( mConnectionCountBand )
    mConnectionCountBand->deleteLater();
  if ( mConnectionBoundaryBand )
    mConnectionBoundaryBand->deleteLater();
  if ( mMaximumSlopeBand )
    mMaximumSlopeBand->deleteLater();
  if ( mMinimumAreaBand )
    mMinimumAreaBand->deleteLater();
  if ( mMaximumAreaBand )
    mMaximumAreaBand->deleteLater();
  if ( mMaximumAreaChangeBand )
    mMaximumAreaChangeBand->deleteLater();

  delete ui;
}

bool ReosEditMeshElementWidget::topographyDisplayed() const
{
  return mDisplayTopograhy->value();
}

void ReosEditMeshElementWidget::hideEvent( QHideEvent *e )
{
  mMapToolEditMeshFrame->deactivate();
  for ( int i = 0; i < mRubberBands.count(); ++i )
    mRubberBands.at( i )->setVisible( false );
  QWidget::hideEvent( e );
}

void ReosEditMeshElementWidget::showEvent( QShowEvent *e )
{
  mMapToolEditMeshFrame->activate();
  mMapToolEditMeshFrame->setCurrentToolInMap();

  QWidget::showEvent( e );
  if ( ui->mAutoUpdateCheckBox->isChecked() )
    startCheckQualityNonControlled();
}

void ReosEditMeshElementWidget::onQualityCheckColorChanged()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/edit-mesh-element/minimum-angle-color" ),
                     ui->mColorMinimumAngle->color() );

  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-angle-color" ),
                     ui->mColorMaximumAngle->color() );

  settings.setValue( QStringLiteral( "/edit-mesh-element/connection-count" ),
                     ui->mColorConnectionCount->color() );

  settings.setValue( QStringLiteral( "/edit-mesh-element/connection-boundary" ),
                     ui->mColorConnectionBoundary->color() );

  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-slope" ),
                     ui->mColorMaximumSlope->color() );

  settings.setValue( QStringLiteral( "/edit-mesh-element/minimum-area" ),
                     ui->mColorMinimumArea->color() );

  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-area" ),
                     ui->mColorMaximumArea->color() );

  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-area-change" ),
                     ui->mColorMaximumAreaChange->color() );
}

void ReosEditMeshElementWidget::onQualityCheckBoxChanged( bool isChecked )
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/edit-mesh-element/auto-update-check" ), ui->mAutoUpdateCheckBox->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/minimum-angle-check" ), ui->mCheckBoxMinimumAngle->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-angle-check" ), ui->mCheckBoxMaximumAngle->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/connection-check" ), ui->mCheckBoxConnectionCount->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/connection-boundary-check" ), ui->mCheckBoxConnectionBoundary->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-slope-check" ), ui->mCheckBoxMaximumSlope->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/minimum-area-check" ), ui->mCheckBoxMinimumArea->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-area-check" ), ui->mCheckBoxMaximumArea->isChecked() );
  settings.setValue( QStringLiteral( "/edit-mesh-element/maximum-area-change-check" ), ui->mCheckBoxMaximumAreaChange->isChecked() );

  if ( isChecked && ui->mAutoUpdateCheckBox->isChecked() )
    startCheckQualityNonControlled();
}

void ReosEditMeshElementWidget::onParameterValueChanged()
{
  if ( ui->mAutoUpdateCheckBox->isChecked() )
    startCheckQualityNonControlled();
}

void ReosEditMeshElementWidget::onMeshChanged()
{
  if ( ui->mAutoUpdateCheckBox->isChecked() && ui->mQualityGroupBox->isChecked() )
    startCheckQualityNonControlled();
}

void ReosEditMeshElementWidget::startCheckQualityNonControlled()
{
  startCheckQuality( false );
}

void ReosEditMeshElementWidget::startCheckQualityControlled()
{
  startCheckQuality( true );
}

void ReosEditMeshElementWidget::startCheckQuality( bool controled )
{
  ReosMesh::QualityMeshChecks checks;

  if ( ui->mCheckBoxMinimumAngle->isChecked() )
    checks |= ReosMesh::MinimumAngle;
  if ( ui->mCheckBoxMaximumAngle->isChecked() )
    checks |= ReosMesh::MaximumAngle;
  if ( ui->mCheckBoxConnectionCount->isChecked() )
    checks |= ReosMesh::ConnectionCount;
  if ( ui->mCheckBoxConnectionBoundary->isChecked() )
    checks |= ReosMesh::ConnectionCountBoundary;
  if ( ui->mCheckBoxMaximumSlope->isChecked() )
    checks |= ReosMesh::MaximumSlope;
  if ( ui->mCheckBoxMinimumArea->isChecked() )
    checks |= ReosMesh::MinimumArea;
  if ( ui->mCheckBoxMaximumArea->isChecked() )
    checks |= ReosMesh::MaximumArea;
  if ( ui->mCheckBoxMaximumAreaChange->isChecked() )
    checks |= ReosMesh::MaximumAreaChange;

  if ( mChecker )
  {
    disconnect( mChecker, &ReosProcess::finished, this, &ReosEditMeshElementWidget::checkQualityFinished );
    mChecker->stop( true );
    connect( mChecker, &ReosProcess::finished, mChecker, &QObject::deleteLater );
  }

  mTimer.restart();
  mChecker = mMesh->getQualityChecker( checks, mGuiContext.map()->mapCrs() );
  connect( mChecker, &ReosProcess::finished, this, &ReosEditMeshElementWidget::checkQualityFinished );

  if ( controled )
  {
    ReosProcessControler *newControler = new ReosProcessControler( mChecker, this );
    newControler->exec();
    newControler->deleteLater();
  }
  else
    mChecker->startOnOtherThread();

}

void ReosEditMeshElementWidget::checkQualityFinished()
{
  qDebug() << "check quality finished " << mChecker->result().error << " in " << mTimer.elapsed();
  qDebug() << "Minimum area " << mChecker->result().minimumArea.count();
  qDebug() << "Maximum area " << mChecker->result().maximumArea.count();
  qDebug() << "Maximum area change " << mChecker->result().maximumAreaChange.count();
  qDebug() << "Minimum angle " << mChecker->result().minimumAngle.count();
  qDebug() << "Maximum angle " << mChecker->result().maximumAngle.count();
  qDebug() << "Connection count" << mChecker->result().connectionCount.count();
  qDebug() << "Connection count boundary" << mChecker->result().connectionCountBoundary.count();
  qDebug() << "Maximum slope" << mChecker->result().maximumSlope.count();
  disconnect( mChecker, &ReosProcess::finished, this, &ReosEditMeshElementWidget::checkQualityFinished );

  if ( mChecker->isSuccessful() )
  {
    ReosApplication::setOverrideCursor( Qt::WaitCursor );

    //! check quality rendered as polygons
    typedef QPair<int, QList<QPolygonF>> Polys  ;
    QList<Polys> polyList;
    polyList << Polys( mRubberBands.indexOf( mMinimumAngleRubberBand ), mChecker->result().minimumAngle )
             << Polys( mRubberBands.indexOf( mMaximumAngleRubberBand ), mChecker->result().maximumAngle )
             << Polys( mRubberBands.indexOf( mMinimumAreaBand ), mChecker->result().minimumArea )
             << Polys( mRubberBands.indexOf( mMaximumAreaBand ), mChecker->result().maximumArea )
             << Polys( mRubberBands.indexOf( mMaximumAreaChangeBand ), mChecker->result().maximumAreaChange );


    for ( auto &poly : std::as_const( polyList ) )
    {
      int polyIndex = poly.first;
      mRubberBands.at( polyIndex )->reset( QgsWkbTypes::PolygonGeometry );
      const QList<QPolygonF> &polys = poly.second;
      if ( !polys.isEmpty() )
      {
        mOnMapButtons.at( polyIndex )->setEnabled( true );
        QgsGeometry faceGeometrie = QgsGeometry::fromQPolygonF( polys.at( 0 ) );
        if ( polys.count() == 1 )
        {
          mRubberBands.at( polyIndex )->setToGeometry( faceGeometrie );
        }
        else
        {
          std::unique_ptr<QgsGeometryEngine> geomEngine( QgsGeometry::createGeometryEngine( faceGeometrie.constGet() ) );
          geomEngine->prepareGeometry();

          QVector<QgsGeometry> otherFaces( polys.count() );
          for ( int i = 0; i < polys.count(); ++i )
            otherFaces[i] = QgsGeometry::fromQPolygonF( polys.at( i ) );
          QString error;
          const QgsGeometry allFaces( geomEngine->combine( otherFaces, &error ) );
          mRubberBands.at( polyIndex )->setToGeometry( allFaces );
        }
      }
      else
      {
        mOnMapButtons.at( polyIndex )->setEnabled( false );
      }
    }

    //! check quality rendered as lines
    typedef QPair<int, QList<QLineF>> Lines;
    QList<Lines> linesList;
    linesList << Lines( {mRubberBands.indexOf( mMaximumSlopeBand ), mChecker->result().maximumSlope} );
    for ( auto &lines : std::as_const( linesList ) )
    {
      int lineIndex = lines.first;
      mRubberBands.at( lineIndex )->reset( QgsWkbTypes::LineGeometry );
      const QList<QLineF> &mapLines = lines.second;
      if ( !mapLines.isEmpty() )
      {
        mOnMapButtons.at( lineIndex )->setEnabled( true );
        const QLineF &l = mapLines.at( 0 );
        QgsGeometry lineGeometrie = QgsGeometry::fromPolyline( {QgsPoint( l.p1() ), QgsPoint( l.p2() ) } );
        if ( mapLines.count() == 1 )
        {
          mRubberBands.at( lineIndex )->setToGeometry( lineGeometrie );
        }
        else
        {
          std::unique_ptr<QgsGeometryEngine> geomEngine( QgsGeometry::createGeometryEngine( lineGeometrie.constGet() ) );
          geomEngine->prepareGeometry();

          QVector<QgsGeometry> otherLines( mapLines.count() );
          for ( int i = 0; i < mapLines.count(); ++i )
          {
            const QLineF &ol = mapLines.at( i );
            otherLines[i] = QgsGeometry::fromPolyline( {QgsPoint( ol.p1() ), QgsPoint( ol.p2() ) } );
          }
          QString error;
          const QgsGeometry allLines( geomEngine->combine( otherLines, &error ) );
          mRubberBands.at( lineIndex )->setToGeometry( allLines );
        }
      }
      else
      {
        mOnMapButtons.at( lineIndex )->setEnabled( false );
      }
    }

    //! check quality rendered as points
    typedef QPair<int, QList<QPointF>> Points;
    QList<Points> pointsList;
    pointsList << Points( {mRubberBands.indexOf( mConnectionCountBand ), mChecker->result().connectionCount} )
               << Points( {mRubberBands.indexOf( mConnectionBoundaryBand ), mChecker->result().connectionCountBoundary} );

    for ( auto &points : std::as_const( pointsList ) )
    {
      int pointIndex = points.first;
      mRubberBands.at( pointIndex )->reset( QgsWkbTypes::PointGeometry );
      const QList<QPointF> &mapPoints = points.second;
      if ( !mapPoints.isEmpty() )
      {
        mOnMapButtons.at( pointIndex )->setEnabled( true );
        const QPointF &pt = mapPoints.at( 0 );
        QgsGeometry lineGeometrie = QgsGeometry::fromQPointF( pt );
        if ( mapPoints.count() == 1 )
        {
          mRubberBands.at( pointIndex )->setToGeometry( lineGeometrie );
        }
        else
        {
          std::unique_ptr<QgsGeometryEngine> geomEngine( QgsGeometry::createGeometryEngine( lineGeometrie.constGet() ) );
          geomEngine->prepareGeometry();

          QVector<QgsGeometry> otherPoints( mapPoints.count() );
          for ( int i = 0; i < mapPoints.count(); ++i )
          {
            const QPointF &opt = mapPoints.at( i );
            otherPoints[i] = QgsGeometry::fromQPointF( opt );
          }
          QString error;
          const QgsGeometry allPoints( geomEngine->combine( otherPoints, &error ) );
          mRubberBands.at( pointIndex )->setToGeometry( allPoints );
        }
      }
      else
      {
        mOnMapButtons.at( pointIndex )->setEnabled( false );
      }
    }

    ReosApplication::restoreOverrideCursor();
  }

  mChecker->deleteLater();
  mChecker = nullptr;

  for ( int i = 0; i < mRubberBands.count(); ++i )
    mRubberBands.at( i )->setVisible( isVisible() && ui->mQualityGroupBox->isChecked() && mCheckBoxes.at( i )->isChecked() );
}
