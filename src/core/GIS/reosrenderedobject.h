/***************************************************************************
  reosrenderedobject.h - ReosRenderedObject

 ---------------------
 begin                : 6.3.2022
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
#ifndef REOSRENDEREDOBJECT_H
#define REOSRENDEREDOBJECT_H

#include <QImage>
#include <QPointer>

#include "reosdataobject.h"
#include "reosprocess.h"

class QGraphicsView;
class QPainter;
class QgsMapCanvas;
class ReosRenderedObject;

class REOSCORE_EXPORT ReosObjectRenderer: public ReosProcess
{
    Q_OBJECT
  public:
    ReosObjectRenderer( ReosRenderedObject *object );

    void start() override;
    void stop( bool stop ) override;

    virtual bool isRenderingStopped() const = 0;

    virtual void render() const = 0;
    const QImage image() const;

    QRectF extent() const;
    void setExtent( const QRectF &extent );

    QDateTime startTime() const;
    void setStartTime( const QDateTime &startTime );

    ReosRenderedObject *object();

  protected:
    QImage mImage;

    virtual void stopRendering() = 0;

  private:
    ReosRenderedObject *mObject = nullptr;
    QRectF mExtent;
    QDateTime mStartTime;

};

class REOSCORE_EXPORT ReosRenderedObject: public ReosDataObject
{
    Q_OBJECT
  public:
    ReosRenderedObject( QObject *parent );
    virtual ReosObjectRenderer *createRenderer( QGraphicsView *view ) = 0;

  signals:
    void renderingFinished();
    void repaintRequested();

};


#endif // REOSRENDEREDOBJECT_H
