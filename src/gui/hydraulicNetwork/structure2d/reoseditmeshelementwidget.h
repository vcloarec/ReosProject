/***************************************************************************
  reoseditmeshelementwidget.h - ReosEditMeshElementWidget

 ---------------------
 begin                : 9.3.2022
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
#ifndef REOSEDITMESHELEMENTWIDGET_H
#define REOSEDITMESHELEMENTWIDGET_H

#include <QWidget>
#include <QPointer>
#include <QElapsedTimer>

#include "reosguicontext.h"

namespace Ui
{
  class ReosEditMeshElementWidget;
}

class QToolBar;
class QCheckBox;
class QToolButton;

class QgsRubberBand;

class ReosMesh;
class ReosMapToolEditMeshFrame;
class ReosParameterDouble;
class ReosParameterInteger;
class ReosParameterSlope;
class ReosParameterArea;
class ReosMeshQualityChecker;
class ReosColorButton;
class ReosParameterWidget;


class ReosEditMeshElementWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosEditMeshElementWidget( ReosMesh *mesh, const ReosGuiContext &context = ReosGuiContext() );
    ~ReosEditMeshElementWidget();


  protected:
    void hideEvent( QHideEvent *e );
    void showEvent( QShowEvent *e );

  private slots:
    void onQualityCheckColorChanged();
    void onQualityCheckBoxChanged( bool isChecked );
    void onParameterValueChanged();
    void onMeshChanged();
    void startCheckQualityNonControlled();
    void startCheckQualityControlled();
    void checkQualityFinished();

  private:
    Ui::ReosEditMeshElementWidget *ui;
    QPointer<ReosMesh> mMesh;
    ReosGuiContext mGuiContext;
    QToolBar *mToolBar = nullptr;
    QAction *mActionEditMeshFrame = nullptr;
    ReosMapToolEditMeshFrame *mMapToolEditMeshFrame = nullptr;
    QList<QCheckBox *> mCheckBoxes;
    QList<ReosColorButton *> mColorButton;
    QList<QgsRubberBand *> mRubberBands;
    QList<QToolButton *> mOnMapButtons;
    QList<ReosParameterWidget *> mParamWidgets;

    //*** quality check
    QElapsedTimer mTimer;
    ReosMeshQualityChecker *mChecker = nullptr;
    QPointer<QgsRubberBand> mMinimumAngleRubberBand;
    QPointer<QgsRubberBand> mMaximumAngleRubberBand;
    QPointer<QgsRubberBand> mConnectionCountBand;
    QPointer<QgsRubberBand> mConnectionBoundaryBand;
    QPointer<QgsRubberBand> mMaximumSlopeBand;
    QPointer<QgsRubberBand> mMinimumAreaBand;
    QPointer<QgsRubberBand> mMaximumAreaBand;
    QPointer<QgsRubberBand> mMaximumAreaChangeBand;

    void startCheckQuality( bool controled );

};

#endif // REOSEDITMESHELEMENTWIDGET_H
