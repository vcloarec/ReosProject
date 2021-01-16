/***************************************************************************
  reosplotpicker_p.h - ReosPlotPicker_p

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
#ifndef REOSPLOTPICKER_P_H
#define REOSPLOTPICKER_P_H

#include <QAction>
#include <QKeyEvent>
#include <QEvent>

#include <qwt_plot_picker.h>
#include <qwt_picker_machine.h>

#include "reosplot_p.h"


class ReosPlotPicker_p: public QwtPlotPicker
{
    Q_OBJECT
  public:
    ReosPlotPicker_p( ReosPlot_p *plot );

    void setAction( QAction *act );
    bool eventFilter( QObject *watched, QEvent *event ) override;

  public slots:
    virtual void activate();
    virtual void deactivate();

    void update();

  signals:
    void activated( ReosPlotPicker_p *picker );
    void deactivated();

  protected:
    virtual void askForStop();


  private:
    QAction *mAction = nullptr;
    virtual void setCursor() {}
};


class ReosPickerMachineLineOneAfterOne_p: public QwtPickerMachine
{
  public:
    ReosPickerMachineLineOneAfterOne_p();
    virtual QList<Command> transition( const QwtEventPattern &eventPattern, const QEvent *event ) override;
};


class ReosPlotPickerDrawLines_p: public ReosPlotPicker_p
{
    Q_OBJECT
  public:
    ReosPlotPickerDrawLines_p( ReosPlot_p *plot );

  protected:
    bool end( bool ok = true ) override;
    virtual void append( const QPoint &pos ) override;
    virtual void move( const QPoint &pos ) override;
    virtual QPolygon adjustedPoints( const QPolygon & ) const override;

  private:
    void setCursor() override;
    QPolygonF mSelectedPoint;
};


class ReosPlotPickerEditPoint_p: public ReosPlotPicker_p
{
    Q_OBJECT
  public:
    ReosPlotPickerEditPoint_p( ReosPlot_p *plot );

    bool eventFilter( QObject *watched, QEvent *event ) override;

    int cursorSize() const;
    void setCursorSize( int value );

  signals:
    void rightClick( const QRectF &pos );
    void purposeBeginMove( const QRectF &pos );
    void moveFinished();

  protected:
    bool end( bool ok = true ) override;

  private:
    int mCursorSize = 8;
    virtual void setCursor() override;

  private slots:
    void beginMove( const QPointF &pos );
};

#endif // REOSPLOTPICKER_P_H
