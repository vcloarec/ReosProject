/***************************************************************************
                      reosmap.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSMAP_H
#define REOSMAP_H

#include <qgsproject.h>

#include "../Reos/reosmodule.h"
#include "../Reos/reosencodedelement.h"

#include "reosmaptool.h"



class HdCursorPosition;

class ReosMap: public ReosModule
{
    Q_OBJECT
  public:
    ReosMap( QObject *parent = nullptr );
    ~ReosMap() override;

    QgsMapCanvas *getMapCanvas() const;

    void setMapTool( ReosMapTool *tool );
    ReosMapTool *getMaptool() const;

    QgsCoordinateReferenceSystem getCoordinateReferenceSystem();

    QWidget *getCursorPosition();

    QRectF getMapExtent() const {return canvas_->extent().toRectF();}
    void setMapExtent( QRectF extent );

    QByteArray encode() const;
    void decode( QByteArray &byteArray );
    void setToSaveExtent();
    void saveMapExtent();
    void setMapSavedExtent( QRectF extent );



  public slots:
    void unsetMapTool( ReosMapTool *tool );
    void unsetMapTool();
    void stopMapTool()
    {
      if ( currentMapTool )
      {
        bool inProgress = currentMapTool->isInProgress();
        currentMapTool->askForEscape();
        if ( !inProgress )
          unsetMapTool();
      }

    }
    void askUnsetMapTool();
    void refreshMap();
    void crsChanged();

  private:
    QgsMapCanvas *canvas_;
    HdCursorPosition *cursorPosition;
    HdMapToolNeutral *mapToolNeutral;
    ReosMapTool *currentMapTool = nullptr;
    QgsRectangle savedExtent;

  public:

    // ReosModule interface
    QWidget *getWidget() const override;

};


class HdCursorPosition : public QLabel
{
    Q_OBJECT
  public:
    HdCursorPosition( QgsMapCanvas *canvas );
    ~HdCursorPosition()
    {
    }

  private:
    QgsMapCanvas *canvas_;

  public slots:
    void actualisePosition( QgsPointXY p );

};



#endif // REOSMAP_H
