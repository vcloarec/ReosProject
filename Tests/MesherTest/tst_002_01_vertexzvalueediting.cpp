#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>


#include "../../Mesher/ReosMesh/reosmesh.h"
#include "../../Mesher/ReosMesh/reosmeshgenerator.h"


using namespace testing;

class VertexZSpecifierTesting:public Test{
public:


    VertexBasic vert1=VertexBasic(0,0);
    VertexBasic vert2=VertexBasic(5,0);

    VertexZSpecifierSimpleFactory simpleZSpecifierFactory;
    VertexZSpecifierOtherVertexAndSlopeFactory slopeZSpecifierFactory;
    VertexZSpecifierOtherVertexAndGapFactory gapZSpecifierFactory;
    VertexZSpecifierInterpolationFactory interpolationZSpecifierFactory;

    // Test interface
protected:
    void SetUp() override
    {
        simpleZSpecifierFactory.setZValue(5);
        vert1.setZSpecifier(simpleZSpecifierFactory);
    }

    void TEST_createVertexSlopeSpecifier()
    {
        slopeZSpecifierFactory.setSlope(0.05);
        slopeZSpecifierFactory.setOtherVertex(&vert1);
        vert2.setZSpecifier(slopeZSpecifierFactory);

        ASSERT_THAT(abs(vert2.z()-5.25),Lt(std::numeric_limits<double>::min()));
    }

    void TEST_createVertexGapSpecifier()
    {
        gapZSpecifierFactory.setGap(0.05);
        gapZSpecifierFactory.setOtherVertex(&vert1);
        vert2.setZSpecifier(gapZSpecifierFactory);

        ASSERT_THAT(abs(vert2.z()-5.05),Lt(std::numeric_limits<double>::min()));
    }

    void TEST_createVertexInterpolationSpecifier()
    {
        interpolationZSpecifierFactory.setExtremitiesVertices(&vert1,&vert2);
    }

};


TEST_F(VertexZSpecifierTesting, verticesDistance){

    auto vertA=VertexBasic(0,0);
    auto vertB=VertexBasic(3,4);

    ASSERT_THAT(abs(vertA.distanceFrom(vertB)-5),Lt(std::numeric_limits<double>::min()));
}

TEST_F(VertexZSpecifierTesting, createSimpleSpecifierDefault){

    vert1.setZSpecifier(simpleZSpecifierFactory);

    ASSERT_THAT(vert1.z(),Eq(5));
}

TEST_F(VertexZSpecifierTesting, createSimpleSpecifierWithValue){


    simpleZSpecifierFactory.setZValue(5);
    vert1.setZSpecifier(simpleZSpecifierFactory);

    ASSERT_THAT(vert1.z(),Eq(5));
}

TEST_F(VertexZSpecifierTesting, createVertexSlopeSpecifier){

    TEST_createVertexSlopeSpecifier();

}

TEST_F(VertexZSpecifierTesting, slopeSpecifier_changeOtherVertexZValue){

    TEST_createVertexSlopeSpecifier();

    simpleZSpecifierFactory.setZValue(10);
    vert1.setZSpecifier(simpleZSpecifierFactory);

    ASSERT_THAT(abs(vert2.z()-10.25),Lt(std::numeric_limits<double>::min()));

}

TEST_F(VertexZSpecifierTesting, slopeSpecifier_changeOtherVertexPosition){

    TEST_createVertexSlopeSpecifier();

    vert1.setPosition(4,0);

    ASSERT_THAT(abs(vert2.z()-5.05),Lt(std::numeric_limits<double>::min()));

}

TEST_F(VertexZSpecifierTesting, createVertexGapSpecifier){

    TEST_createVertexGapSpecifier();
}


TEST_F(VertexZSpecifierTesting, createVertexGapSpecifier_changeOtherVertexZValue){

    TEST_createVertexGapSpecifier();

    simpleZSpecifierFactory.setZValue(10);
    vert1.setZSpecifier(simpleZSpecifierFactory);

    ASSERT_THAT(abs(vert2.z()-10.05),Lt(std::numeric_limits<double>::min()));
}


