package com.test

import cats.data.NonEmptyList
import scalavro.avsc._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineMV
import org.scalatest.FlatSpec

class ParserTest extends FlatSpec {

  "Parser" should "handle ex2" in {
//    AvroADTParser.buildAllClassesAsStr(ex2).foreach(println)
  }
  it should "handle ex3" in {
//    AvroADTParser.buildAllClassesAsStr(ex3)
  }
  val ex2 = Record(
    refineMV[NonEmpty]("User"),
    Some(refineMV[NonEmpty]("com.example.avro")),
    None,
    None,
    NonEmptyList(
      Field(refineMV[NonEmpty]("id"), None, IntType),
      List(
        Field(refineMV[NonEmpty]("username"), None, StringType),
        Field(refineMV[NonEmpty]("passwordHash"), None, StringType),
        Field(refineMV[NonEmpty]("signupDate"), None, LongType),
        Field(
          refineMV[NonEmpty]("emailAddresses"),
          None,
          ArrayType(
            Record(
              refineMV[NonEmpty]("EmailAddress"),
              None,
              None,
              None,
              NonEmptyList(
                Field(refineMV[NonEmpty]("address"), None, StringType),
                List(
                  Field(refineMV[NonEmpty]("verified"), None, BoolType),
                  Field(refineMV[NonEmpty]("dateAdded"), None, LongType),
                  Field(refineMV[NonEmpty]("dateBounced"), None, Union(NonEmptyList(NullType, List(LongType)))),
                )
              )
            ))
        ),
        Field(
          refineMV[NonEmpty]("twitterAccounts"),
          None,
          ArrayType(
            Record(
              refineMV[NonEmpty]("TwitterAccount"),
              None,
              None,
              None,
              NonEmptyList(
                Field(
                  refineMV[NonEmpty]("status"),
                  None,
                  EnumType(
                    refineMV[NonEmpty]("OAuthStatus"),
                    None,
                    None,
                    None,
                    NonEmptyList(
                      refineMV[NonEmpty]("PENDING"),
                      List(
                        refineMV[NonEmpty]("ACTIVE"),
                        refineMV[NonEmpty]("DENIED"),
                        refineMV[NonEmpty]("EXPIRED"),
                        refineMV[NonEmpty]("REVOKED")
                      )
                    )
                  )
                ),
                List(
                  Field(refineMV[NonEmpty]("userId"), None, LongType),
                  Field(refineMV[NonEmpty]("screenName"), None, StringType),
                  Field(refineMV[NonEmpty]("oauthToken"), None, StringType),
                  Field(refineMV[NonEmpty]("oauthTokenSecret"), None, Union(NonEmptyList(NullType, List(StringType)))),
                  Field(refineMV[NonEmpty]("dateAuthorized"), None, LongType),
                )
              )
            ))
        ),
        Field(
          refineMV[NonEmpty]("toDoItems"),
          None,
          ArrayType(
            Record(
              refineMV[NonEmpty]("ToDoItem"),
              None,
              None,
              None,
              NonEmptyList(
                Field(
                  refineMV[NonEmpty]("status"),
                  None,
                  EnumType(
                    refineMV[NonEmpty]("ToDoStatus"),
                    None,
                    None,
                    None,
                    NonEmptyList(
                      refineMV[NonEmpty]("HIDDEN"),
                      List(
                        refineMV[NonEmpty]("ACTIONABLE"),
                        refineMV[NonEmpty]("DONE"),
                        refineMV[NonEmpty]("ARCHIVED"),
                        refineMV[NonEmpty]("DELETED")
                      )
                    )
                  )
                ),
                List(
                  Field(refineMV[NonEmpty]("title"), None, StringType),
                  Field(refineMV[NonEmpty]("description"), None, Union(NonEmptyList(NullType, List(StringType)))),
                  Field(refineMV[NonEmpty]("snoozeDate"), None, Union(NonEmptyList(NullType, List(LongType)))),
                  Field(refineMV[NonEmpty]("subItems"), None, ArrayType(RecordByName("ToDoItem")))
                )
              )
            ))
        )
      )
    )
  )
  val ex3 = Record(
    refineMV[NonEmpty]("Unions"),
    Some(refineMV[NonEmpty]("com.example.avro")),
    None,
    None,
    NonEmptyList(
      Field(refineMV[NonEmpty]("union1"), None, Union(NonEmptyList(StringType, List(IntType)))),
      List(
        Field(
          refineMV[NonEmpty]("union2"),
          None,
          Union(NonEmptyList(NullType, List(IntType)))
        ),
        Field(
          refineMV[NonEmpty]("union3"),
          None,
          Union(NonEmptyList(NullType, List(IntType, StringType)))
        ),
        Field(
          refineMV[NonEmpty]("union4"),
          None,
          Union(
            NonEmptyList(
              NullType,
              List(
                Record(
                  refineMV[NonEmpty]("r1"),
                  None,
                  None,
                  None,
                  NonEmptyList(
                    Field(refineMV[NonEmpty]("int"), None, IntType),
                    List.empty
                  )
                )
              )
            )
          )
        ),
        Field(
          refineMV[NonEmpty]("union4"),
          None,
          Union(
            NonEmptyList(
              NullType,
              List(
                StringType,
                Record(
                  refineMV[NonEmpty]("r2"),
                  None,
                  None,
                  None,
                  NonEmptyList(
                    Field(refineMV[NonEmpty]("int"), None, IntType),
                    List.empty
                  )
                )
              )
            )
          )
      )
    ))
  )
}
