/***************************************************************************
  reosverticalexaggerationwidget.h - ReosVerticalExaggerationWidget

 ---------------------
 begin                : 14.3.2022
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
#ifndef REOSVERTICALEXAGGERATIONWIDGET_H
#define REOSVERTICALEXAGGERATIONWIDGET_H

#include <QWidget>
#include <QSlider>

namespace Ui
{
  class ReosVerticalExaggerationWidget;
}

//! Derived clas from QSlider that represent a slider that has the cursor returning to initial position when user release mouse button
class ReosSliderElastic : public QSlider
{
    Q_OBJECT
  public :
    ReosSliderElastic( QWidget *parent ): QSlider( parent ) {}

  signals:
    //! Emitted when the user release the slider
    void finalValue( double value );

  protected:
    void mouseReleaseEvent( QMouseEvent *ev ) override;
};

class ReosVerticalExaggerationWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosVerticalExaggerationWidget( QWidget *parent = nullptr );
    ~ReosVerticalExaggerationWidget();

    void setExageration( double exageration );
    double exageration() const;

  signals:
    void valueChanged( double value ) const;

  private:
    Ui::ReosVerticalExaggerationWidget *ui;

    double mExaggeration = 1.0;
    double mPreviousValue = 1.0;

    double exagerationFromSliderValue( int value ) const;
};

#endif // REOSVERTICALEXAGGERATIONWIDGET_H
