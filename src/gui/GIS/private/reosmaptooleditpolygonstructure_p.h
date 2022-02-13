/***************************************************************************
  reosmaptooleditpolygonstructure_p.h - ReosMapToolEditPolygonStructure_p

 ---------------------
 begin                : 6.2.2022
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
#ifndef REOSMAPTOOLEDITPOLYGONSTRUCTURE_P_H
#define REOSMAPTOOLEDITPOLYGONSTRUCTURE_P_H

#include "reosmaptool_p.h"

#include "reospolygonstructure.h"

class QgsMapCanvas;

class ReosMapToolEditPolygonStructure_p : public ReosMapTool_p
{
  public:
    ReosMapToolEditPolygonStructure_p( QgsMapCanvas *map );

    void setStructure( ReosPolygonStructure *structure );
    QActionGroup *mainActions() const;

    void setCurrentClassId( const QString &currentClassId );

  protected:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    //void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
    void keyPressEvent( QKeyEvent *e ) override;

  private:
    QPointer<ReosPolygonStructure> mStructure;
    QgsRubberBand *mPolygonRubberBand = nullptr;

    QActionGroup *mMainActionsGroup = nullptr;
    QAction *mActionRedo = nullptr;
    QAction *mActionUndo = nullptr;

    QString mCurrentClassId;

    void resetTool();
    void addPolygon( const QPolygonF &polygon );

};

#endif // REOSMAPTOOLEDITPOLYGONSTRUCTURE_P_H
