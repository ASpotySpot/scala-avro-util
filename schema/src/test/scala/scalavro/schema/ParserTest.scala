package scalavro.schema

import cats.data.NonEmptyList
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineMV
import org.scalatest.FlatSpec
import scalavro.schema.types.AvscType._

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
      Field(refineMV[NonEmpty]("id"), None, IntType)(None),
      List(
        Field(refineMV[NonEmpty]("username"), None, StringType)(None),
        Field(refineMV[NonEmpty]("passwordHash"), None, StringType)(None),
        Field(refineMV[NonEmpty]("signupDate"), None, LongType)(None),
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
                Field(refineMV[NonEmpty]("address"), None, StringType)(None),
                List(
                  Field(refineMV[NonEmpty]("verified"), None, BoolType)(None),
                  Field(refineMV[NonEmpty]("dateAdded"), None, LongType)(None),
                  Field(refineMV[NonEmpty]("dateBounced"), None, Union(NullType, List(LongType)))(None),
                )
              )
            )
          )
        )(None),
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
                )(None),
                List(
                  Field(refineMV[NonEmpty]("userId"), None, LongType)(None),
                  Field(refineMV[NonEmpty]("screenName"), None, StringType)(None),
                  Field(refineMV[NonEmpty]("oauthToken"), None, StringType)(None),
                  Field(refineMV[NonEmpty]("oauthTokenSecret"), None, Union(NullType, List(StringType)))(None),
                  Field(refineMV[NonEmpty]("dateAuthorized"), None, LongType)(None),
                )
              )
            )
          )
        )(None),
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
                )(None),
                List(
                  Field(refineMV[NonEmpty]("title"), None, StringType)(None),
                  Field(refineMV[NonEmpty]("description"), None, Union(NullType, List(StringType)))(None),
                  Field(refineMV[NonEmpty]("snoozeDate"), None, Union(NullType, List(LongType)))(None),
                  Field(refineMV[NonEmpty]("subItems"), None, ArrayType(RecordByName(refineMV[NonEmpty]("ToDoItem"))))(None)
                )
              )
            )
          )
        )(None)
      )
    )
  )
  val ex3 = Record(
    refineMV[NonEmpty]("Unions"),
    Some(refineMV[NonEmpty]("com.example.avro")),
    None,
    None,
    NonEmptyList(
      Field(refineMV[NonEmpty]("union1"), None, Union(StringType, List(IntType)))(None),
      List(
        Field(
          refineMV[NonEmpty]("union2"),
          None,
          Union(NullType, List(IntType))
        )(None),
        Field(
          refineMV[NonEmpty]("union3"),
          None,
          Union(NullType, List(IntType, StringType)),
        )(None),
        Field(
          refineMV[NonEmpty]("union4"),
          None,
          Union(
            NullType,
            List(
              Record(
                refineMV[NonEmpty]("r1"),
                None,
                None,
                None,
                NonEmptyList(
                  Field(refineMV[NonEmpty]("int"), None, IntType)(None),
                  List.empty
                )
              )

            )
          )
        )(None),
        Field(
          refineMV[NonEmpty]("union4"),
          None,
          Union(
            NullType,
            List(
              StringType,
              Record(
                refineMV[NonEmpty]("r2"),
                None,
                None,
                None,
                NonEmptyList(
                  Field(refineMV[NonEmpty]("int"), None, IntType)(None),
                  List.empty
                )
              )
            )
          )
        )(None)
      ))
  )
}
