#ifndef TEST_H
#define TEST_H


#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Triangulation_2.h>
#include <CGAL/Constrained_Delaunay_triangulation_2.h>
#include <CGAL/Constrained_triangulation_plus_2.h>


typedef CGAL::Exact_predicates_inexact_constructions_kernel                 K;
typedef CGAL::Polygon_2<K>                                                Polygon_2;
typedef CGAL::Exact_intersections_tag                                     Itag_;
typedef CGAL::Constrained_Delaunay_triangulation_2<K,CGAL::Default, Itag_> CDT;
typedef CGAL::Constrained_triangulation_plus_2<CDT>                       CDTP;

typedef CDTP::Point                                                       Point;
typedef CDTP::Constraint_id                                               Cid;
typedef CDTP::Vertex_handle                                               Vertex_handle;


bool testCDTP()
{
    CDTP cdtp;

    Vertex_handle handle1=cdtp.insert(Point(0,0));
    Vertex_handle handle2=cdtp.insert(Point(0,10));
    Vertex_handle handle3=cdtp.insert(Point(10,10));
    Vertex_handle handle4=cdtp.insert(Point(10,0));

    auto constraintId=cdtp.insert_constraint(handle1,handle3);
    //cdtp.remove_constraint(constraintId);
    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;
    std::cout<<"number of enclosing constraints "<<cdtp.number_of_enclosing_constraints(handle1,handle3)<<std::endl;



    auto lastConstraintId=cdtp.insert_constraint(handle1,handle3);
    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;
    std::cout<<"number of enclosing constraints "<<cdtp.number_of_enclosing_constraints(handle1,handle3)<<std::endl;


    cdtp.remove_constraint(cdtp.context(handle1,handle3).id());
    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;
    std::cout<<"number of enclosing constraints "<<cdtp.number_of_enclosing_constraints(handle1,handle3)<<std::endl;

    cdtp.remove_constraint(cdtp.context(handle1,handle3).id());
    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;
    if (cdtp.is_subconstraint(handle1,handle3))
        std::cout<<"number of enclosing constraints "<<cdtp.number_of_enclosing_constraints(handle1,handle3)<<std::endl;
    else
        std::cout<<"number of enclosing constraints "<<"no constraints"<<std::endl;

    return (lastConstraintId != nullptr);

}


int countVertex(CDTP &cdtp, CDTP::Constraint_id id)
{
    auto v=cdtp.vertices_in_constraint_begin(id);

    int count=0;
    while(v!=cdtp.vertices_in_constraint_end(id))
    {
        count++;
        v++;
    }

    return count;
}

bool testCDTP_2()
{
    CDTP cdtp;

    Vertex_handle handle1=cdtp.insert(Point(0,0));
    Vertex_handle handle2=cdtp.insert(Point(0,10));
    Vertex_handle handle3=cdtp.insert(Point(6,0));
    Vertex_handle handle4=cdtp.insert(Point(6,10));
    Vertex_handle handle5=cdtp.insert(Point(8,0));
    Vertex_handle handle6=cdtp.insert(Point(8,10));

    Vertex_handle handle7=cdtp.insert(Point(-1,5));
    Vertex_handle handle8=cdtp.insert(Point(9,5));

    //Vertex_handle handle9=cdtp.insert(Point(0,5));


//    auto ctId1=cdtp.insert_constraint(handle1,handle2);
//    auto ctId2=cdtp.insert_constraint(handle3,handle4);
//    auto ctId3=cdtp.insert_constraint(handle5,handle6);
    auto ctId4=cdtp.insert_constraint(handle7,handle8);


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctId4)<<std::endl;


    auto ctId1=cdtp.insert_constraint(handle1,handle2);
    auto ctId2=cdtp.insert_constraint(handle3,handle4);
    auto ctId3=cdtp.insert_constraint(handle5,handle6);

    cdtp.remove_constraint(ctId3);
    cdtp.remove_constraint(ctId2);
    cdtp.remove_constraint(ctId1);

    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;

    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctId4)<<std::endl;

    auto vertexToRemove=cdtp.vertices_in_constraint_begin(ctId4);
    vertexToRemove++;
    vertexToRemove++;
    cdtp.remove_vertex_from_constraint(ctId4,vertexToRemove);

    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;

    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctId4)<<std::endl;

    return true;

}


bool testCDTP_3()
{
    CDTP cdtp;

    Vertex_handle handle1=cdtp.insert(Point(0,0));
    Vertex_handle handle2=cdtp.insert(Point(0,1));
    Vertex_handle handle3=cdtp.insert(Point(0,2));
    Vertex_handle handle4=cdtp.insert(Point(0,3));
    Vertex_handle handle5=cdtp.insert(Point(0,4));
    Vertex_handle handle6=cdtp.insert(Point(0,5));

    Vertex_handle handle7=cdtp.insert(Point(0,-1));
    Vertex_handle handle8=cdtp.insert(Point(0,6));

    auto ctId=cdtp.insert_constraint(handle7,handle8);


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctId)<<std::endl;


    auto vertexToRemove=cdtp.vertices_in_constraint_begin(ctId);
    vertexToRemove++;
    vertexToRemove++;

    std::cout<<"************************************************"<<std::endl;
    std::cout<<"attempt to remove vertex "<<(*vertexToRemove)->point().x()<<" , "<<(*vertexToRemove)->point().x();
    cdtp.remove_vertex_from_constraint(ctId,vertexToRemove);


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctId)<<std::endl;

        return true;

}

bool testCDTP_4()
{
    CDTP cdtp;

    std::list<Point> pointsListColinear;

    pointsListColinear.push_back(Point(0,0));
    pointsListColinear.push_back(Point(0,1));
    pointsListColinear.push_back(Point(0,2));
    pointsListColinear.push_back(Point(0,3));
    pointsListColinear.push_back(Point(0,4));
    pointsListColinear.push_back(Point(0,5));

    std::list<Point> pointsListNoColinear;

    pointsListNoColinear.push_back(Point(1,0));
    pointsListNoColinear.push_back(Point(2,1));
    pointsListNoColinear.push_back(Point(4,2));
    pointsListNoColinear.push_back(Point(7,3));
    pointsListNoColinear.push_back(Point(11,4));
    pointsListNoColinear.push_back(Point(16,5));


    auto ctIdColinear=cdtp.insert_constraint(pointsListColinear.begin(),pointsListColinear.end());
    auto ctIdNoColinear=cdtp.insert_constraint(pointsListNoColinear.begin(),pointsListNoColinear.end());


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctIdColinear)<<std::endl;

    auto vertexToRemoveColinear=cdtp.vertices_in_constraint_begin(ctIdColinear);
    vertexToRemoveColinear++;

    std::cout<<"************************************************"<<std::endl;
    std::cout<<"attempt to remove vertex "<<(*vertexToRemoveColinear)->point().x()<<" , "<<(*vertexToRemoveColinear)->point().x();
    cdtp.remove_vertex_from_constraint(ctIdColinear,vertexToRemoveColinear);


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctIdColinear)<<std::endl;


    std::cout<<"**************************************************************************************************************************************";

    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctIdNoColinear)<<std::endl;

    auto vertexToRemoveNoColinear=cdtp.vertices_in_constraint_begin(ctIdNoColinear);
    vertexToRemoveNoColinear++;

    std::cout<<"************************************************"<<std::endl;
    std::cout<<"attempt to remove vertex "<<(*vertexToRemoveNoColinear)->point().x()<<" , "<<(*vertexToRemoveNoColinear)->point().x();
    cdtp.remove_vertex_from_constraint(ctIdNoColinear,vertexToRemoveNoColinear);


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of subconstraints "<<cdtp.number_of_subconstraints()<<std::endl;
    std::cout<<"number of constraints "<<cdtp.number_of_constraints()<<std::endl;


    std::cout<<"*********************************************************************"<<std::endl;
    std::cout<<"number of vertex in constraint "<<countVertex(cdtp,ctIdNoColinear)<<std::endl;


        return true;
}




#endif // TEST_H
