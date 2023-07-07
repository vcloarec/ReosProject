/***************************************************************************
  reosgriddedrainfallexportoptionswidget.cpp - ReosGriddedRainfallExportOptionsWidget

 ---------------------
 begin                : 16.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosgriddedrainfallexportoptionswidget.h"
#include "ui_reosgriddedrainfallexportoptionswidget.h"

#include "reosmemoryraster.h"
#include "reosmaptool.h"
#include "reosgisengine.h"
#include "reosgriddedrainitem.h"

ReosGriddedRainfallExportOptionsWidget::ReosGriddedRainfallExportOptionsWidget( QWidget *parent )
  : QWidget( parent )
  , ui( new Ui::ReosGriddedRainfallExportOptionsWidget )
  , mNorthParam( new ReosParameterDouble( QString(), this ) )
  , mSouthParam( new ReosParameterDouble( QString(), this ) )
  , mWestParam( new ReosParameterDouble( QString(), this ) )
  , mEastParam( new ReosParameterDouble( QString(), this ) )
  , mHorizontalResolution( new ReosParameterDouble( QString(), this ) )
  , mVerticalResolution( new ReosParameterDouble( QString(), this ) )
{
  ui->setupUi( this );
  ui->mNorth->setDouble( mNorthParam );
  ui->mSouth->setDouble( mSouthParam );
  ui->mWest->setDouble( mWestParam );
  ui->mEast->setDouble( mEastParam );
  ui->mResolutionHori->setDouble( mHorizontalResolution );
  ui->mResolutionVerti->setDouble( mVerticalResolution );

  connect( ui->mUse00Origin, &QCheckBox::clicked, this, [this]
  {
    ui->mRadioButtonBottomLeft->setEnabled( !ui->mUse00Origin->isChecked() );
    ui->mRadioButtonTopLeft->setEnabled( !ui->mUse00Origin->isChecked() );
    ui->mRadioButtonBottomRight->setEnabled( !ui->mUse00Origin->isChecked() );
    ui->mRadioButtonTopRight->setEnabled( !ui->mUse00Origin->isChecked() );
  } );

  ui->mRadioButtonBottomLeft->setEnabled( !ui->mUse00Origin->isChecked() );
  ui->mRadioButtonTopLeft->setEnabled( !ui->mUse00Origin->isChecked() );
  ui->mRadioButtonBottomRight->setEnabled( !ui->mUse00Origin->isChecked() );
  ui->mRadioButtonTopRight->setEnabled( !ui->mUse00Origin->isChecked() );

  connect( ui->mAdjustResolution, &QToolButton::clicked, this, &ReosGriddedRainfallExportOptionsWidget::onAdjustResolution );
  connect( ui->mAdjustExtent, &QToolButton::clicked, this, &ReosGriddedRainfallExportOptionsWidget::onAdjustExtent );

  connect( ui->mRainFallExtent, &QToolButton::clicked, this, [this]
  {
    setExtent( mRainfallExtent );
  } );

  connect( ui->mCrsWidget, &ReosCoordinateSystemWidget::crsChanged, this, &ReosGriddedRainfallExportOptionsWidget::reprojectExtent );

  connect( ui->mStartDateTimeEdit, &QDateTimeEdit::dateTimeChanged, this, &ReosGriddedRainfallExportOptionsWidget::onTimeFromChanged );
  connect( ui->mEndDateTimeEdit, &QDateTimeEdit::dateTimeChanged, this, &ReosGriddedRainfallExportOptionsWidget::onTimeToChanged );
}

ReosGriddedRainfallExportOptionsWidget::~ReosGriddedRainfallExportOptionsWidget()
{
  delete ui;
}

void ReosGriddedRainfallExportOptionsWidget::syncRainfall( ReosGriddedRainfall *rainfall )
{
  setExtent( rainfall->rasterExtent() );

  int count = rainfall->gridCount();

  ui->mStartDateTimeEdit->setDateTime( rainfall->startTime( 0 ) );
  ui->mEndDateTimeEdit->setDateTime( rainfall->endTime( count - 1 ) );
}

void ReosGriddedRainfallExportOptionsWidget::setSupportedGridOrigin( ReosGriddedRainfallProvider::SupportedGridOrigins origins )
{
  bool onlyZeroOrigin = origins.testFlag( ReosGriddedRainfallProvider::ZeroBottomLeft ) &&
                        ( !( origins.testFlag( ReosGriddedRainfallProvider::TopLeft ) ||
                             origins.testFlag( ReosGriddedRainfallProvider::TopRight ) ||
                             origins.testFlag( ReosGriddedRainfallProvider::BottomLeft ) ||
                             origins.testFlag( ReosGriddedRainfallProvider::BottomRight ) ) );

  ui->mUse00Origin->setEnabled( !onlyZeroOrigin && origins.testFlag( ReosGriddedRainfallProvider::ZeroBottomLeft ) );
  ui->mRadioButtonTopLeft->setEnabled( origins.testFlag( ReosGriddedRainfallProvider::TopLeft ) );
  ui->mRadioButtonTopRight->setEnabled( origins.testFlag( ReosGriddedRainfallProvider::TopRight ) );
  ui->mRadioButtonBottomLeft->setEnabled( origins.testFlag( ReosGriddedRainfallProvider::BottomLeft ) );
  ui->mRadioButtonBottomRight->setEnabled( origins.testFlag( ReosGriddedRainfallProvider::BottomRight ) );

  ui->mUse00Origin->setChecked( onlyZeroOrigin );

  onAdjustExtent();
}

void ReosGriddedRainfallExportOptionsWidget::setExtent( const ReosRasterExtent &extent )
{
  ui->mCrsWidget->setCrs( extent.crs() );
  mCurrentCrs = extent.crs();
  mRainfallExtent = extent;

  int decimals = ui->mCrsWidget->bestDecimalNumber();

  mNorthParam->setDisplayPrecision( decimals );
  mSouthParam->setDisplayPrecision( decimals );
  mWestParam->setDisplayPrecision( decimals );
  mEastParam->setDisplayPrecision( decimals );

  mHorizontalResolution->setDisplayPrecision( decimals );
  mVerticalResolution->setDisplayPrecision( decimals );

  mNorthParam->setValue( extent.yMapMax() );
  mSouthParam->setValue( extent.yMapMin() );
  mWestParam->setValue( extent.xMapMin() );
  mEastParam->setValue( extent.xMapMax() );

  mHorizontalResolution->setValue( std::abs( extent.xCellSize() ) );
  mVerticalResolution->setValue( std::abs( extent.yCellSize() ) );

  if ( extent.xCellSize() > 0 && extent.yCellSize() > 0 )
  {
    ui->mRadioButtonBottomLeft->setChecked( true );
    ui->mRadioButtonTopLeft->setChecked( false );
    ui->mRadioButtonBottomRight->setChecked( false );
    ui->mRadioButtonTopRight->setChecked( false );
  }

  if ( extent.xCellSize() > 0 && extent.yCellSize() < 0 )
  {
    ui->mRadioButtonBottomLeft->setChecked( false );
    ui->mRadioButtonTopLeft->setChecked( true );
    ui->mRadioButtonBottomRight->setChecked( false );
    ui->mRadioButtonTopRight->setChecked( false );
  }

  if ( extent.xCellSize() < 0 && extent.yCellSize() < 0 )
  {
    ui->mRadioButtonBottomLeft->setChecked( false );
    ui->mRadioButtonTopLeft->setChecked( false );
    ui->mRadioButtonBottomRight->setChecked( false );
    ui->mRadioButtonTopRight->setChecked( true );
  }

  if ( extent.xCellSize() < 0 && extent.yCellSize() > 0 )
  {
    ui->mRadioButtonBottomLeft->setChecked( false );
    ui->mRadioButtonTopLeft->setChecked( false );
    ui->mRadioButtonBottomRight->setChecked( true );
    ui->mRadioButtonTopRight->setChecked( false );
  }
}

ReosRasterExtent ReosGriddedRainfallExportOptionsWidget::rasterExtent() const
{
  ReosRasterExtent ret;
  if ( ui->mUse00Origin->isChecked() )
  {
    int xPixelBottomLeft = std::ceil( mWestParam->value() / mHorizontalResolution->value() );
    int yPixelBottomLeft = std::ceil( mSouthParam->value() / mVerticalResolution->value() );

    int xPixelTopRight = std::floor( mEastParam->value() / mHorizontalResolution->value() );
    int yPixelTopRight = std::floor( mNorthParam->value() / mVerticalResolution->value() );

    int xCount = xPixelTopRight - xPixelBottomLeft + 1;
    int yCount = yPixelTopRight - yPixelBottomLeft + 1;

    ret = ReosRasterExtent( ( xPixelBottomLeft - 1 ) * mHorizontalResolution->value(),
                            ( yPixelBottomLeft - 1 ) * mVerticalResolution->value(),
                            xCount,
                            yCount,
                            mHorizontalResolution->value(),
                            mHorizontalResolution->value() );
  }
  else
  {
    double width = std::abs( mWestParam->value() - mEastParam->value() );
    double height = std::abs( mNorthParam->value() - mSouthParam->value() );

    int xCount = static_cast<int>( std::round( width / std::abs( mHorizontalResolution->value() ) ) );
    int yCount = static_cast<int>( std::round( height / std::abs( mVerticalResolution->value() ) ) );

    if ( ui->mRadioButtonTopLeft->isChecked() )
    {
      ret = ReosRasterExtent(
              mWestParam->value(),
              mNorthParam->value(),
              xCount,
              yCount,
              mHorizontalResolution->value(),
              -mVerticalResolution->value() );
    }
    if ( ui->mRadioButtonBottomLeft->isChecked() )
    {
      ret = ReosRasterExtent(
              mWestParam->value(),
              mSouthParam->value(),
              xCount,
              yCount,
              mHorizontalResolution->value(),
              mVerticalResolution->value() );
    }
    if ( ui->mRadioButtonBottomRight->isChecked() )
    {
      ret = ReosRasterExtent(
              mEastParam->value(),
              mSouthParam->value(),
              xCount,
              yCount,
              -mHorizontalResolution->value(),
              mVerticalResolution->value() );
    }
    if ( ui->mRadioButtonTopRight->isChecked() )
    {
      ret = ReosRasterExtent(
              mEastParam->value(),
              mNorthParam->value(),
              xCount,
              yCount,
              -mHorizontalResolution->value(),
              -mVerticalResolution->value() );
    }

  }

  ret.setCrs( ui->mCrsWidget->crs() );

  return ret;
}

ReosTimeWindow ReosGriddedRainfallExportOptionsWidget::timeWindow() const
{
  return ReosTimeWindow( ui->mStartDateTimeEdit->dateTime(), ui->mEndDateTimeEdit->dateTime() );
}


void ReosGriddedRainfallExportOptionsWidget::reprojectExtent()
{
  const QString newCrs = ui->mCrsWidget->crs();
  ReosMapExtent currentExtent( mWestParam->value(), mSouthParam->value(), mEastParam->value(), mNorthParam->value() );
  currentExtent.setCrs( mCurrentCrs );
  ReosMapExtent newExtent = ReosGisEngine::transformExtent( currentExtent, newCrs );

  double width = std::abs( mWestParam->value() - mEastParam->value() );
  double height = std::abs( mNorthParam->value() - mSouthParam->value() );

  int xCount = static_cast<int>( std::round( width / std::abs( mHorizontalResolution->value() ) ) );
  int yCount = static_cast<int>( std::round( height / std::abs( mVerticalResolution->value() ) ) );

  mHorizontalResolution->setValue( newExtent.width() / xCount );
  mVerticalResolution->setValue( newExtent.height() / yCount );

  mWestParam->setValue( newExtent.xMapMin() );
  mEastParam->setValue( newExtent.xMapMax() );
  mNorthParam->setValue( newExtent.yMapMax() );
  mSouthParam->setValue( newExtent.yMapMin() );
  mCurrentCrs = newCrs;
}

void ReosGriddedRainfallExportOptionsWidget::onAdjustResolution()
{
  if ( ui->mUse00Origin->isChecked() )
  {
    int xPixelBottomLeft = static_cast<int>( std::round( mWestParam->value() / mHorizontalResolution->value() ) );
    int yPixelBottomLeft = static_cast<int>( std::round( mSouthParam->value() / mVerticalResolution->value() ) );

    if ( xPixelBottomLeft != 0 )
      mHorizontalResolution->setValue( mWestParam->value() / xPixelBottomLeft );
    if ( yPixelBottomLeft != 0 )
      mVerticalResolution->setValue( mSouthParam->value() / yPixelBottomLeft );
  }
  else
  {
    double width = std::abs( mWestParam->value() - mEastParam->value() );
    double height = std::abs( mNorthParam->value() - mSouthParam->value() );

    int xCount = static_cast<int>( std::round( width / std::abs( mHorizontalResolution->value() ) ) );
    int yCount = static_cast<int>( std::round( height / std::abs( mVerticalResolution->value() ) ) );

    mHorizontalResolution->setValue( width / xCount );
    mVerticalResolution->setValue( height / yCount );
  }
}

void ReosGriddedRainfallExportOptionsWidget::onAdjustExtent()
{
  if ( ui->mUse00Origin->isChecked() )
  {
    int xPixelBottomLeft = std::ceil( mWestParam->value() / mHorizontalResolution->value() );
    int yPixelBottomLeft = std::ceil( mSouthParam->value() / mVerticalResolution->value() );

    int xPixelTopRight = std::floor( mEastParam->value() / mHorizontalResolution->value() );
    int yPixelTopRight = std::floor( mNorthParam->value() / mVerticalResolution->value() );

    int xCount = xPixelTopRight - xPixelBottomLeft;
    int yCount = yPixelTopRight - yPixelBottomLeft;

    mWestParam->setValue( xPixelBottomLeft  * mHorizontalResolution->value() );
    mSouthParam->setValue( yPixelBottomLeft * mVerticalResolution->value() );
    mEastParam->setValue( mWestParam->value() + xCount * mHorizontalResolution->value() );
    mNorthParam->setValue( mSouthParam->value() + yCount * mVerticalResolution->value() );
  }
  else
  {
    double width = std::abs( mWestParam->value() - mEastParam->value() );
    double height = std::abs( mNorthParam->value() - mSouthParam->value() );

    int xCount = static_cast<int>( std::round( width / std::abs( mHorizontalResolution->value() ) ) );
    int yCount = static_cast<int>( std::round( height / std::abs( mVerticalResolution->value() ) ) );

    if ( ui->mRadioButtonTopLeft->isChecked() )
    {
      mEastParam->setValue( mWestParam->value() + xCount * mHorizontalResolution->value() );
      mSouthParam->setValue( mNorthParam->value() - yCount * mVerticalResolution->value() );
    }

    if ( ui->mRadioButtonBottomLeft->isChecked() )
    {
      mEastParam->setValue( mWestParam->value() + xCount * mHorizontalResolution->value() );
      mNorthParam->setValue( mSouthParam->value() + yCount * mVerticalResolution->value() );
    }

    if ( ui->mRadioButtonTopRight->isChecked() )
    {
      mWestParam->setValue( mWestParam->value() - xCount * mHorizontalResolution->value() );
      mSouthParam->setValue( mNorthParam->value() - yCount * mVerticalResolution->value() );
    }

    if ( ui->mRadioButtonBottomRight->isChecked() )
    {
      mWestParam->setValue( mEastParam->value() - xCount * mHorizontalResolution->value() );
      mNorthParam->setValue( mSouthParam->value() + yCount * mVerticalResolution->value() );
    }
  }


}

void ReosGriddedRainfallExportOptionsWidget::onTimeFromChanged()
{
  const QDateTime from = ui->mStartDateTimeEdit->dateTime();
  const QDateTime to = ui->mEndDateTimeEdit->dateTime();
  if ( from >= to )
    ui->mEndDateTimeEdit->setDateTime( from );
}

void ReosGriddedRainfallExportOptionsWidget::onTimeToChanged()
{
  const QDateTime from = ui->mStartDateTimeEdit->dateTime();
  const QDateTime to = ui->mEndDateTimeEdit->dateTime();
  if ( from >= to )
    ui->mStartDateTimeEdit->setDateTime( to );
}
