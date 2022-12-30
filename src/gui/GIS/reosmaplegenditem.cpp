/***************************************************************************
  reosmaplegenditem.cpp - ReosMapLegendItem

 ---------------------
 begin                : 20.11.2022
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
#include "reosmaplegenditem.h"

#include <qgsmapcanvas.h>
#include <qgstextrenderer.h>
#include <QDebug>

#include "reosmap.h"
#include "reosmappolygon_p.h"
#include "reosrenderersettings.h"



ReosColorRampMapLegendItem::ReosColorRampMapLegendItem( ReosColorShaderSettings *settings )
  : mSettings( settings )
{
}

QRectF ReosColorRampMapLegendItem::boundingRect() const
{
  return mBoundingRect;
}

void ReosColorRampMapLegendItem::paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * )
{
  QgsRenderContext renderContext;
  renderContext.setPainter( painter );
  renderContext.setScaleFactor( painter->device()->logicalDpiX() / 25.4 );
  renderContext.setTextRenderFormat( Qgis::TextRenderFormat::AlwaysText );

  int phyDpiX = painter->device()->physicalDpiX();
  int phyDpiY = painter->device()->physicalDpiY();

  if ( mSettings .isNull() || !mSettings->isValid() )
    return;

  painter->save();

  double rampBoxWidth = mRampBoxWidth / 25.4 * phyDpiX;
  double horiSpacing = mHorSpacing / 25.4 * phyDpiX;
  double vertSpacing = mVertSpacing / 25.4 * phyDpiY;

  QgsTextFormat format;
  QgsTextBufferSettings bufferSettings;
  bufferSettings.setSize( 1 );
  bufferSettings.setColor( Qt::white );
  bufferSettings.setEnabled( true );
  format.setBuffer( bufferSettings );
  format.setSizeUnit( QgsUnitTypes::RenderPixels );
  format.setSize( 15 );
  QFont font = format.font();
  font.setBold( true );
  format.setFont( font );
  QFontMetricsF metrics = QgsTextRenderer::fontMetrics( renderContext, format );

  QString legendTitle = mSettings->title();
  QRectF titleBox = metrics.boundingRect( mBoundingRect.adjusted( 0, 0, -horiSpacing / 2, 0 ), Qt::AlignRight | Qt::AlignTop | Qt::TextWordWrap, legendTitle );
  QgsTextRenderer::drawText( titleBox,
                             0,
                             Qgis::TextHorizontalAlignment::Right,
                             QStringList() << legendTitle,
                             renderContext,
                             format,
                             true,
                             Qgis::TextVerticalAlignment::VerticalCenter,
                             Qgis::TextRendererFlag::WrapLines );

  format.setSize( 15 );
  font = format.font();
  font.setBold( false );
  format.setFont( font );

  metrics = QgsTextRenderer::fontMetrics( renderContext, format );

  double leftGradientBox = ( mBoundingRect.right() - horiSpacing - rampBoxWidth );

  const QString textTopLabel = QLocale().toString( mSettings->classificationMaximum(), 'f', 2 );
  QRectF topLabelBbox = metrics.boundingRect( textTopLabel );
  topLabelBbox.translate( leftGradientBox - topLabelBbox.width() - topLabelBbox.left() - horiSpacing / 2,
                          titleBox.bottom() + vertSpacing / 2 - topLabelBbox.top() );

  const QString textBottomLabel = QLocale().toString( mSettings->classificationMinimum(), 'f', 2 );
  QRectF bottomLabelBbox = metrics.boundingRect( textBottomLabel );
  bottomLabelBbox.translate( leftGradientBox - bottomLabelBbox.width() - bottomLabelBbox.left() - horiSpacing / 2,
                             mBoundingRect.bottom() - bottomLabelBbox.bottom() - vertSpacing );

  double topGradientBox = ( topLabelBbox.top() + topLabelBbox.bottom() ) / 2;
  double bottomGradiantBox = ( bottomLabelBbox.bottom() + bottomLabelBbox.top() ) / 2;

  if ( topGradientBox < bottomGradiantBox )
  {
    QgsTextRenderer::drawText( topLabelBbox, 0, Qgis::TextHorizontalAlignment::Left, QStringList() << textTopLabel, renderContext, format, true, Qgis::TextVerticalAlignment::VerticalCenter );
    QgsTextRenderer::drawText( bottomLabelBbox, 0, Qgis::TextHorizontalAlignment::Left, QStringList() << textBottomLabel, renderContext, format, true, Qgis::TextVerticalAlignment::VerticalCenter );

    //double rightGradientBox = mBoundingRect.right() - mHorSpacing;
    QLinearGradient gradient = mSettings->gradient();
    gradient.setStart( leftGradientBox, bottomGradiantBox );
    gradient.setFinalStop( leftGradientBox, topGradientBox );
    painter->setBrush( gradient );
    painter->drawRect( QRectF( leftGradientBox,
                               topGradientBox,
                               rampBoxWidth, bottomGradiantBox - topGradientBox ) );
  }

  painter->restore();

#if 0
  painter->drawRect( mBoundingRect );
  painter->drawRect( titleBox );
  painter->drawRect( topLabelBbox );
  painter->drawRect( bottomLabelBbox );
#endif
}

void ReosColorRampMapLegendItem::setOrder( int order )
{
  mOrder = order;
}

void ReosColorRampMapLegendItem::setLegendCount( int newLegendCount )
{
  mLegendCount = newLegendCount;
}

void ReosColorRampMapLegendItem::resize( QWidget *viewport )
{
  prepareGeometryChange();
  double viewHeight = viewport->size().height();
  double viewWidth = viewport->size().width();

  int phyDpiX = viewport->physicalDpiX();
  int phyDpiY = viewport->physicalDpiY();

  double legendHeigh = std::min( viewHeight / mLegendCount - 30, mSizeHint.height() / 25.4 * phyDpiY );
  double legendWidth = mSizeHint.width() / 25.4 * phyDpiX;

  QPointF legendTopLeft = QPointF( viewWidth - legendWidth, viewHeight - legendHeigh * ( mOrder + 1 ) );

  QRectF legendBox = QRectF( legendTopLeft, QSizeF( legendWidth, legendHeigh ) );
  mBoundingRect = legendBox;
}

