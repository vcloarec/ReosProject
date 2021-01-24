/***************************************************************************
  reosplot_p.h - ReosPlot_p

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
#ifndef REOSPLOT_P_H
#define REOSPLOT_P_H

#include <memory>
#include <qwt_plot.h>
#include <qwt_plot_magnifier.h>

class QwtPlotLegendItem;
class QwtPlotGrid;
class ReosPlotItem;
class QwtPlotMagnifier;
class QwtPlotPanner;
class QwtPlotZoomer;
class QwtPlotPicker;
class QwtPlotItem;


class ReosPositiveMagnifier : public QwtPlotMagnifier
{
  public:
    ReosPositiveMagnifier( QWidget *canvas );
    void setYMinimumEnabled( bool b );

  protected:
    virtual void rescale( double factor ) override;

  private:
    bool mIsYMinEnabeled;
};

class ReosPlot_p: public QwtPlot
{
    Q_OBJECT
  public:
    ReosPlot_p( QWidget *parent = nullptr );

    void setLegendVisible( bool b );
    void setLegendAlignement( Qt::Alignment align );
    void enableAutoMinimumSize( bool b );
    void setMinimumPlotSize( QSize minSize );
    void setRightAxeEnabled( bool b );

    void setEnableZoomer( bool b );

    void setNormalMagnifier();
    void setPositiveMagnifier();

    QSize sizeHint() const override;
    QSize minimumSizeHint() const override;

    //! Adds an item to the plot, takes ownership
    void addItem( ReosPlotItem *item );

  signals:
    void reploted();

  public slots:
    void replot() override
    {
      QwtPlot::replot();
      emit reploted();
    }

  private:
    QwtPlotGrid *mGrid = nullptr;
    QwtPlotLegendItem *mLegend = nullptr;
    QSize mMinimumSize = QSize( 300, 225 );
    bool mAutoMinimumSizeIsEnable = false;
    std::vector<std::unique_ptr<ReosPlotItem>> mItems;

    QwtPlotMagnifier *mMagnifier = nullptr;
    QwtPlotPanner *mPanner = nullptr;
    QwtPlotZoomer *mZoomerLeft = nullptr;
    QwtPlotZoomer *mZoomerRight = nullptr;
};
#endif // REOSPLOT_P_H
