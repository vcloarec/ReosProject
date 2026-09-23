/***************************************************************************
  reosmaptooleditpolygonsclassified_p.h - ReosMapToolEditPolygonsClassified_p

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
#ifndef REOSMAPTOOLEDITPOLYGONSCLASSIFIED_P_H
#define REOSMAPTOOLEDITPOLYGONSCLASSIFIED_P_H

#include "reosmaptool_p.h"

#include "reospolygonsclassified.h"

class QgsMapCanvas;
class ReosMapToolEditPolygonsClassified_p;

class ReosEditPolygonStructureMenuPopulator : public ReosMenuPopulator
{
  public:
    ReosEditPolygonStructureMenuPopulator( ReosMapToolEditPolygonsClassified_p *toolMap );

    bool populate( QMenu *menu, QgsMapMouseEvent *e = nullptr ) override;

  private:
    ReosMapToolEditPolygonsClassified_p *mToolMap = nullptr;
};

class ReosMapToolEditPolygonsClassified_p : public ReosMapTool_p
{
  public:
    ReosMapToolEditPolygonsClassified_p( QgsMapCanvas *map );

    Flags flags() const override;

    void setStructure( ReosPolygonsClassified *structure );
    QActionGroup *mainActions() const;

    void setCurrentClassId( const QString &currentClassId );
    void addHelperStructure( ReosGeometryComplex *structure );

    void activate() override;
    void deactivate() override;

  protected:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    //void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
    void keyPressEvent( QKeyEvent *e ) override;

  private:
    enum State
    {
      None,
      AddingPolygon,
    };

    State mCurrentState = None;
    QPointF mCurrentPosition;
    QPointer<ReosPolygonsClassified> mStructure;
    QgsRubberBand *mPolygonRubberBand = nullptr;

    QActionGroup *mMainActionsGroup = nullptr;
    QAction *mActionRedo = nullptr;
    QAction *mActionUndo = nullptr;

    QString mCurrentClassId;

    void resetTool();
    void addPolygon( const QPolygonF &polygon );

    bool hasHelperPolygon() const;

    QList<QPointer<ReosGeometryComplex>> mHelperStructure;

    friend class ReosEditPolygonStructureMenuPopulator;
};

#endif // REOSMAPTOOLEDITPOLYGONSCLASSIFIED_P_H
