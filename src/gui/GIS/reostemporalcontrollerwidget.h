/***************************************************************************
  reostemporalcontrollerwidget.h - ReosTemporalControllerWidget

 ---------------------
 begin                : 18.6.2022
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
#ifndef REOSTEMPORALCONTROLLERWIDGET_H
#define REOSTEMPORALCONTROLLERWIDGET_H

#include <QWidget>


class ReosTemporalController_p;
class ReosDuration;

namespace Ui
{
  class ReosTemporalControllerWidget;
}

class ReosTemporalControllerWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosTemporalControllerWidget( QWidget *parent = nullptr );
    ~ReosTemporalControllerWidget();

    QObject *temporalController();

  private slots:
    void speedFactorChanged( double speedFactor );
    void onSliderMoved( int value );

  private:
    Ui::ReosTemporalControllerWidget *ui;

    ReosTemporalController_p *mTemporalController = nullptr;

    void activatePlay();
    void activatePlayBack();
    void activatePause();

    void setCurrentTime( const QDateTime &time );
};

#endif // REOSTEMPORALCONTROLERWIDGET_H
