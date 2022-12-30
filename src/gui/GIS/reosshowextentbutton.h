/***************************************************************************
  reosshowextentbutton.h - ReosShowExtentButton

 ---------------------
 begin                : 29.12.2022
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
#ifndef REOSSHOWEXTENTBUTTON_H
#define REOSSHOWEXTENTBUTTON_H

#include <QToolButton>

#include "reosmapextent.h"

class ReosMap;
class ReosMapPolygon;

class ReosShowExtentButton : public QToolButton
{
  public:
    explicit ReosShowExtentButton( QWidget *parent );

    void setExtent( const ReosMapExtent &newExtent );

    void setMap( ReosMap *map );

  private:
    void onPressed();
    void onReleased();
  private:
    ReosMap *mMap = nullptr;
    std::unique_ptr<ReosMapPolygon> mMapExtent;
    ReosMapExtent mExtent;
};

#endif // REOSSHOWEXTENTBUTTON_H
