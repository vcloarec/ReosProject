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


class QPainter;
class ReosRenderedObject;
class ReosRendererSettings;
class ReosColorShaderSettings;
class ReosMapExtent;

#ifndef SIP_RUN

class ReosRendererObjectMapTimeStamp
{
  public:
    virtual ~ReosRendererObjectMapTimeStamp() = default;
    virtual bool equal( ReosRendererObjectMapTimeStamp *other ) = 0;
};

class REOSCORE_EXPORT ReosObjectRenderer: public ReosProcess
{
    Q_OBJECT
  public:
    ReosObjectRenderer( ReosRenderedObject *object );
    ~ReosObjectRenderer();

    void start() override;
    void stop( bool stop ) override;

    virtual bool isRenderingStopped() const = 0;

    virtual void render() = 0;
    const QImage image() const;

    QRectF extent() const;
    void setExtent( const QRectF &extent );

    //! Sets the map time stamp for which this object is associted with. Used to recognize identical time step renderer during rendering
    void setMapTimeStamp( ReosRendererObjectMapTimeStamp *timeStamp );

    //! Returns a pointer to the timeStamp, the caller take onwership of the time stamp.
    ReosRendererObjectMapTimeStamp *releaseMapTimeStamp();

    //! Returns a pointer to the timeStamp, owner ship is kept by the renderer.
    ReosRendererObjectMapTimeStamp *mapTimeStamp() const;

    ReosRenderedObject *object();

  protected:
    QImage mImage;

    virtual void stopRendering() = 0;

  private:
    QPointer<ReosRenderedObject> mObject;
    QRectF mExtent;
    std::unique_ptr<ReosRendererObjectMapTimeStamp> mMapTimeStamp;

};
#endif //SIP_RUN

/**
 * Base classe of data object rendered on the map (for example mesh)
 */
class REOSCORE_EXPORT ReosRenderedObject: public ReosDataObject  SIP_ABSTRACT
{
    Q_OBJECT
  public:

    //! Constructor
    ReosRenderedObject( QObject *parent = nullptr );

    /**
     * Creates usable renderer settings from a pointer to external renderer settings (pointer to
     * an instance of QgsMapSettings with QGIS core library).
     */
    static std::unique_ptr<ReosRendererSettings> createRenderSettings( const void *settings ) SIP_SKIP;

    /**
     * Creates an instance of a map time stamp that is used to recognize identical time step rendering for the specific rendered object
     * The caller takes ownership of the instance.
     */
    virtual ReosRendererObjectMapTimeStamp *createMapTimeStamp( ReosRendererSettings *settings ) const  SIP_SKIP = 0;

    /**
     * Creates an instance of an onject renderer, caller take ownership of the new instance.
     */
    virtual ReosObjectRenderer *createRenderer( ReosRendererSettings *settings )  SIP_SKIP = 0;

    //! Returns all the color shader settings handled by this object
    virtual QList<ReosColorShaderSettings *> colorShaderSettings() const  SIP_SKIP = 0;

    //! Returns the extent of the rendered object
    virtual ReosMapExtent extent() const  SIP_SKIP = 0;

    //! Updates internal cache
    virtual void updateInternalCache( ReosObjectRenderer *renderer ) SIP_SKIP {}

  signals:
    void renderingFinished() SIP_SKIP;
    void repaintRequested() SIP_SKIP;
};

#endif // REOSRENDEREDOBJECT_H
