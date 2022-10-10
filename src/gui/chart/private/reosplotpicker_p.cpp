/***************************************************************************
  reosplotpicker_p.cpp - ReosPlotPicker_p

 ---------------------
 begin                : 14.1.2021
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
#include "reosplotpicker_p.h"



ReosPlotPicker_p::ReosPlotPicker_p( ReosPlot_p *plot ): QwtPlotPicker( plot->canvas() )
{
  connect( plot, &ReosPlot_p::reploted, this, &ReosPlotPicker_p::update );
}

void ReosPlotPicker_p::setAction( QAction *act )
{
  if ( !act )
    return;
  mAction = act;
  connect( mAction, &QAction::triggered, this, &ReosPlotPicker_p::activate );
}

bool ReosPlotPicker_p::eventFilter( QObject *watched, QEvent *event )
{
  if ( watched == canvas() )
  {
    if ( event->type() == QEvent::KeyPress )
    {
      QKeyEvent *keyEvent = static_cast<QKeyEvent *>( event );
      if ( keyEvent->key() == Qt::Key_Escape )
      {
        deactivate();
        return true;
      }
      else
        return QwtPlotPicker::eventFilter( watched, event );
    }
  }

  return QwtPlotPicker::eventFilter( watched, event );
}

void ReosPlotPicker_p::activate()
{
  setEnabled( mAction->isChecked() );
  if ( isEnabled() )
  {
    emit activated( this );
    setCursor();
  }
  else
  {
    deactivate();
  }

}

void ReosPlotPicker_p::deactivate()
{
  if ( mAction )
    mAction->setChecked( false );
  setEnabled( false );
  reset();
  emit deactivated();
  plot()->canvas()->setCursor( Qt::CrossCursor );
}

void ReosPlotPicker_p::update()
{
  updateDisplay();
}

void ReosPlotPicker_p::askForStop()
{
  if ( mAction )
    mAction->setChecked( false );
  reset();
  setEnabled( false );
}

ReosPickerMachineLineOneAfterOne_p::ReosPickerMachineLineOneAfterOne_p():
  QwtPickerMachine( PolygonSelection )
{
}

QList<QwtPickerMachine::Command> ReosPickerMachineLineOneAfterOne_p::transition( const QwtEventPattern &eventPattern, const QEvent *event )
{
  QList<QwtPickerMachine::Command> cmdList;

  switch ( event->type() )
  {
    case QEvent::MouseButtonPress:
    {
      if ( eventPattern.mouseMatch( QwtEventPattern::MouseSelect1,
                                    static_cast<const QMouseEvent *>( event ) ) )
      {
        if ( state() == 0 )
        {
          cmdList += Begin;
          cmdList += Append;
          cmdList += Append;
          setState( 1 );
        }
        else
        {
          cmdList += Append;
        }
      }
      if ( eventPattern.mouseMatch( QwtEventPattern::MouseSelect2,
                                    static_cast<const QMouseEvent *>( event ) ) )
      {
        if ( state() == 1 )
        {
          cmdList += End;
          setState( 0 );
        }
      }
      break;
    }
    case QEvent::MouseMove:
    case QEvent::Wheel:
    {
      if ( state() != 0 )
        cmdList += Move;
      break;
    }
    default:
      break;
  }
  return cmdList;
}

ReosPlotPickerDrawLines_p::ReosPlotPickerDrawLines_p( ReosPlot_p *plot ): ReosPlotPicker_p( plot )
{
  setStateMachine( new ReosPickerMachineLineOneAfterOne_p );
  setRubberBand( QwtPicker::PolygonRubberBand );
}

bool ReosPlotPickerDrawLines_p::end( bool ok )
{
  bool ret = ReosPlotPicker_p::end( ok );
  askForStop();
  mSelectedPoint.clear();
  return ret;
}

void ReosPlotPickerDrawLines_p::append( const QPoint &pos )
{
  QPointF pt = invTransform( pos );
  mSelectedPoint.append( pt );
  emit appended( pt );
}

void ReosPlotPickerDrawLines_p::move( const QPoint &pos )
{
  QPointF pt = invTransform( pos );
  if ( !mSelectedPoint.isEmpty() )
  {
    mSelectedPoint.last() = pt;
    updateDisplay();
  }

  emit moved( pt );
}

QPolygon ReosPlotPickerDrawLines_p::adjustedPoints( const QPolygon & ) const
{
  QPolygon ret;
  for ( const QPointF &pt : mSelectedPoint )
    ret.append( transform( pt ) );

  return ret;
}

void ReosPlotPickerDrawLines_p::setCursor()
{
  plot()->canvas()->setCursor( QCursor( QStringLiteral( ":/cursors/linearDrawing.png" ), 4, 4 ) );
}

ReosPlotPickerEditPoint_p::ReosPlotPickerEditPoint_p( ReosPlot_p *plot ): ReosPlotPicker_p( plot )
{
  setStateMachine( new QwtPickerDragPointMachine );
  connect( this, &QwtPlotPicker::appended, this, &ReosPlotPickerEditPoint_p::beginMove );
}

bool ReosPlotPickerEditPoint_p::eventFilter( QObject *watched, QEvent *event )
{
  if ( watched == canvas() )
  {
    if ( event->type() == QEvent::MouseButtonPress )
    {
      QMouseEvent *mouseEvent = static_cast<QMouseEvent *>( event );
      if ( mouseEvent->button() == Qt::RightButton )
      {
        QPoint pos = mouseEvent->pos();
        QRect rect( pos - QPoint( mCursorSize / 2, mCursorSize / 2 ), pos + QPoint( mCursorSize / 2, mCursorSize / 2 ) );
        emit rightClick( invTransform( rect ) );
        return true;
      }
      else
        return ReosPlotPicker_p::eventFilter( watched, event );
    }
  }

  return ReosPlotPicker_p::eventFilter( watched, event );
}

int ReosPlotPickerEditPoint_p::cursorSize() const
{
  return mCursorSize;
}

bool ReosPlotPickerEditPoint_p::end( bool ok )
{
  bool ret = ReosPlotPicker_p::end( ok );
  emit moveFinished();
  return ret;
}

void ReosPlotPickerEditPoint_p::setCursor()
{
  plot()->canvas()->setCursor( QPixmap( ":/cursors/moveElement.png" ) );
}

void ReosPlotPickerEditPoint_p::beginMove( const QPointF &pos )
{
  QPoint pt = transform( pos );
  QRect rect( pt - QPoint( mCursorSize / 2, mCursorSize / 2 ), pt + QPoint( mCursorSize / 2, mCursorSize / 2 ) );
  emit purposeBeginMove( invTransform( rect ) );
}
