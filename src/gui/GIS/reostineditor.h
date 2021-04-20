/***************************************************************************
  reostineditor.h - ReosTinEditor

 ---------------------
 begin                : 13.4.2021
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
#ifndef REOSTINEDITOR_H
#define REOSTINEDITOR_H

#include "reosmodule.h"
#include "reosmaptool.h"

class ReosGisEngine;
class ReosMap;
class ReosMapToolDrawPoint;
class ReosTriangularIrregularNetwork;
class ReosTinEditorMapToolSelectVertex_p;

class ReosTinEditorMapItems : public ReosMapItem
{
  public:
    ReosTinEditorMapItems( ReosMap *map );
    void setTriangulation( ReosTriangularIrregularNetwork *triangulation );
    void updateItems();
};

class ReosTinEditorMapToolSelectVertex : public ReosMapTool
{
    Q_OBJECT
  public:
    ReosTinEditorMapToolSelectVertex( ReosMap *map, ReosTinEditorMapItems *tinEditorMapItems );

  signals:
    void selectedVertexIndex( int vertexIndex );

  private:
    QPointer<ReosTinEditorMapToolSelectVertex_p> d;
    ReosMapTool_p *tool_p() const;
};

class ReosTinEditor: public ReosModule
{
    Q_OBJECT
  public:
    ReosTinEditor( ReosGisEngine *gisEngine, ReosMap *map, QWidget *parentWidget );
    QList<QAction *> actions() const;

  public slots:
    void setCurrentLayer( const QString &currentLayer );

  private slots:
    void addVertex( const QPointF &vertex );
    void fromVectorLayer();
    void activate( bool b );

  private:
    ReosGisEngine *mGisEngine;
    QWidget *mParentWidget;

    QAction *mActionAddVertex = nullptr;
    QAction *mActionFromVectorLayer = nullptr;
    QAction *mActionRemoveVertex = nullptr;
    ReosTinEditorMapItems mItems;
    ReosTinEditorMapToolSelectVertex *mRemoveVertexMapTool = nullptr;

    ReosTriangularIrregularNetwork *mCurrentTriangulation = nullptr;
};


#endif // REOSTINEDITOR_H
