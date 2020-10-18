/***************************************************************************
                      reoswatershedstore.h
                     --------------------------------------
Date                 : 04-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSWATERSHEDSTORE_H
#define REOSWATERSHEDSTORE_H

#include <vector>
#include <memory>



class ReosWatershed;
class QPolygonF;

class ReosWatershedStore
{
  public:
    ReosWatershedStore();

    //! Adds a watershed to the store, take ownership and return a pointer to the watershed
    ReosWatershed *addWatershed( ReosWatershed *watershed );

    //! Returns the smallest watershed that is downstream the line, if the line is partially included by any watershed, ok is false
    //! If there is no watershed downstrean, return nullptr
    ReosWatershed *downstreamWatershed( const QPolygonF &line, bool &ok ) const;

    int watershedCount() const;
    int masterWatershedCount() const;

  private:

    std::vector<std::unique_ptr<ReosWatershed>> mWatersheds;
};

#endif // REOSWATERSHEDSTORE_H
