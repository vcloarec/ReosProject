/***************************************************************************
  reoseditableprofile.h - ReosEditableProfile

 ---------------------
 begin                : 14.1.2021
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
#ifndef REOSEDITABLEPROFILE_H
#define REOSEDITABLEPROFILE_H

#include "reosplotwidget.h"
#include <QAbstractTableModel>

class ReosPlot_p;
class ReosPlotPicker_p;
class ReosPlotPickerEditPoint_p;
class ReosPlotPickerDrawLines_p;

class ReosEditableProfileModel: public QAbstractTableModel
{
    Q_OBJECT
  public:
    ReosEditableProfileModel( QObject *parent = nullptr );

    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;

    //! Removes all points
    void clear();

    void setPoints( const QPolygonF &points );

    //! Returns the points count
    int pointCount() const;

    //! Returns a reference to the list of point
    const QPolygonF &points() const;

    //! Inserts point at position \a i
    void insertPoint( int i, const QPointF &point );

    //! Adds a point at the end of the profile
    void addPoint( const QPointF &point );

    //! Removes point a index \a i
    void removePoint( int i );

    //! Returns the first point index that is in the rectangle defined by \a rect
    int pointIndexIn( const QRectF &rect ) const;

    //! Moves the point with index \a at the position \a newPos
    void movePoint( int i, const QPointF &newPos );

  signals:
    void pointChanged( int index );
    void pointInserted( int index );
    void pointRemoved( int index );

  private:
    QPolygonF mPoints;

    QString mTemporaryNewX;
    QString mTemporaryNewY;

};


class ReosEditableProfile: public ReosPlotItem
{
    Q_OBJECT
  public:
    //! Contructor
    ReosEditableProfile();

    void attach( ReosPlot_p *plot ) override;

    QAbstractTableModel *tableModel();
    QList<QAction *> actionsToolBar() const;
    QPolygonF profile() const;

  public slots:
    void addPoint( const QPointF &point );
    void setProfile( const QPolygonF &prof );

  signals:
    void profileChanged();

  private slots:
    void beginMove( const QRectF &rect );
    void movePoint( const QPointF &newPos );
    void endMovePoint();
    void createProfileNewPoint( const QPointF &newPos );
    void finishProfile( const QVector<QPointF> &pts );
    void pickerActivated( ReosPlotPicker_p *picker );
    void deactivateZoomer();
    void activateZoomer();
    void contextMenuEdition( const QRectF &rect );
    void deletePoint();
    void insertPoint();
    void zoomExtent();

  private:
    ReosEditableProfileModel mModel;
    ReosPlotPicker_p *mCurrentPicker = nullptr;
    ReosPlotPickerEditPoint_p *mPickerEditPoint = nullptr;
    ReosPlotPickerDrawLines_p *mPickerNewProfile = nullptr;
    ReosPlot_p *mPlot;

    int mMovingPointIndex = -1;
    QPolygonF mOldProfile;
    bool mWaitingForFirstPoint = true;

    int mContextMenuTargetPointIndex = -1;
    QPointF mContextMenuPoint;

    QAction *mActionEditProfile = nullptr;
    QAction *mActionCreateNewProfile = nullptr;
    QActionGroup *mActionGroup;
    QAction *mActionZoomExtent = nullptr;

    QAction *mActionRemovePoint;
    QAction *mActionInsertPoint;
};

#endif // REOSEDITABLEPROFILE_H
