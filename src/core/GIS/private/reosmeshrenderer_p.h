/***************************************************************************
  reosmeshrenderer_p.h - ReosMeshRenderer_p

 ---------------------
 begin                : 30.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSMESHRENDERER_P_H
#define REOSMESHRENDERER_P_H

#include "reosrenderersettings_p.h"
#include "qgsmeshtracerenderer.h"
#include "qgsmeshlayerutils.h"

#include <QThread>
#include <QTimer>

class ReosMeshFrame_p;
class ReosMeshRenderer_p;

struct DynamicTracesSettings
{
  bool dynamicTracesEnable = false;
  int lifeTime = 15;
  int maxSpeed = 200;
  int fps = 15;
  double persistence = 0.2;
  double tailFactor = 1;
};

class ReosMovingTracesRenderer : public QObject
{
    Q_OBJECT
  public:
    ReosMovingTracesRenderer( QgsMeshLayer *layer,
                              int datasetGroupindex,
                              const QgsRenderContext &context,
                              const QgsMeshDataBlock &datasetVectorValues,
                              const QgsMeshDataBlock &scalarActiveFaceFlagValues,
                              double magnitudeMaximum,
                              const DynamicTracesSettings &tracesSettings );

    //! Resets vector data without removing traces data, fo now does nothing, in plave for memory
    void resetVectorDataset( const QgsMeshDataBlock &datasetVectorValues,
                             const QgsMeshDataBlock &scalarActiveFaceFlagValues,
                             double magnitudeMaximum );

    QgsFeedback *feedback() const {return mFeedBack.get();}

  public slots:
    void start();
    void stop();
    void moveParticles();

  signals:
    void imageReady( QImage image, quint64 tracesAge );

  private:
    std::unique_ptr<QgsMeshVectorTraceAnimationGenerator> mTraceGenerator;
    std::unique_ptr<QgsFeedback> mFeedBack;
    QgsRenderContext mRenderContext;
    QPoint mTopLeft;
    QSize mOutputSize;
    int mFramePerSeconds = 10;
    quint64 mTracesAges;
    QTimer *mTimer;
};

class ReosMovingTracesController : public QObject
{
    Q_OBJECT
  public:
    ReosMovingTracesController( QObject *parent = nullptr );

    ~ReosMovingTracesController();

    void resetData( QgsMeshLayer *layer,
                    const QgsRenderContext &context,
                    int vectorDatasetGroupIndex,
                    const DynamicTracesSettings &tracesSettings );

    void start();
    void stop();
    quint64 traceAges() const;

    QImage lastTracesImage() const;

  signals:
    void askStop();
    void imageReady();

  private slots:
    void setLastImage( QImage img, quint64 tracesAge );

  private:
    QThread mThread;
    ReosMovingTracesRenderer *mRenderer = nullptr;
    QImage mLastTracesImage;
    quint64 mTracesAge = 0;
    DynamicTracesSettings mTracesSettings;
};


class ReosMeshRendererCache_p
{
  public:
    ReosMeshRendererCache_p( ReosMeshFrame_p *mesh, int datasetIndex );
    ~ReosMeshRendererCache_p();

    void updateCache( QgsMeshLayer *layer, const QgsRenderContext &renderContext );

    QImage traceImage() const;
    quint64 tracesAges() const;

    const QRectF &extent() const;
    const QImage &staticRendering() const;

    void updateInternalCache( ReosMeshRenderer_p *renderer );

  private:
    ReosMovingTracesController mTraceController;
    QRectF mExtent;
    DynamicTracesSettings mTracesSettings;
    int mDatasetGroupsIndex = -1;
    QImage mStaticRendering;
    QgsMeshDatasetIndex mVectorDataset;
    QgsMeshDatasetIndex mScalarDataset;

    bool updateDataset( QgsMeshLayer *layer, const QgsRenderContext &renderContext );
    bool updateExtent( const QgsRenderContext &renderContext );
};

class ReosMeshRenderer_p : public ReosQgisLayerRenderer_p
{
    Q_OBJECT
  public:
    ReosMeshRenderer_p( ReosRendererSettings *settings, QgsMeshLayer *layer, ReosMeshFrame_p *reosMesh );
    ~ReosMeshRenderer_p();

    void render() override;

    const QImage &staticRendering() const;

  private:
    QImage mStaticRendering;
    QImage mTraceImage;
};

#endif // REOSMESHRENDERER_P_H
