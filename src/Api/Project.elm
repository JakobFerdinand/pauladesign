module Api.Project exposing (Project, ProjectDetails, allProjects, projectDetails)

import DataSource exposing (DataSource)
import DataSource.Http
import OptimizedDecoder as Decode
import Pages.Secrets as Secrets


type alias Project =
    { title : String
    , mainImageUrl : String
    , slug : String
    , projectDescription : Maybe String
    }


allProjects : DataSource (List Project)
allProjects =
    DataSource.Http.get
        (Secrets.succeed "https://htusge0f.api.sanity.io/v1/data/query/production?query=*%5B%20_type%20%3D%3D%20'project'%20%5D%7B%0A%20%20title%2C%0A%20%20%22imageUrl%22%3A%20mainImage.asset-%3Eurl%2C%0A%20%20%22slug%22%3A%20slug.current%2C%0A%20%20%22description%22%3A%20projectDescription%0A%7D%0A%0A")
        (Decode.field "result"
            (Decode.list
                (Decode.map4 Project
                    (Decode.field "title" Decode.string)
                    (Decode.field "imageUrl" Decode.string)
                    (Decode.field "slug" Decode.string)
                    (Decode.maybe (Decode.field "description" Decode.string))
                )
            )
        )


type alias ProjectDetails =
    { title : String
    , mainImageUrl : String
    , imageUrls : List String
    , projectDescription : Maybe String
    , enhancedProjectDescription : Maybe String
    }



-- GROQ Query
-- *[_type == 'project' && slug.current == 'jakobs-projekt'] {
--  title,
--  "mainImageUrl": mainImage.asset->url,
--  projectDescription,
--  "imageUrls": images[].asset->url,
--  enhancedProjectDescription
--}


projectDetails : String -> DataSource ProjectDetails
projectDetails slug =
    DataSource.Http.get
        (Secrets.succeed ("https://htusge0f.api.sanity.io/v1/data/query/production?query=*%5B_type%20%3D%3D%20'project'%20%26%26%20slug.current%20%3D%3D%20'" ++ slug ++ "'%5D%20%7B%0A%20%20title%2C%0A%20%20%22mainImageUrl%22%3A%20mainImage.asset-%3Eurl%2C%0A%20%20projectDescription%2C%0A%20%20%22imageUrls%22%3A%20images%5B%5D.asset-%3Eurl%2C%0A%20%20enhancedProjectDescription%0A%7D"))
        (Decode.field "result"
            (Decode.index
                0
                (Decode.map5 ProjectDetails
                    (Decode.field "title" Decode.string)
                    (Decode.field "mainImageUrl" Decode.string)
                    (Decode.field "imageUrls" (Decode.list Decode.string))
                    (Decode.maybe (Decode.field "projectDescription" Decode.string))
                    (Decode.maybe (Decode.field "enhancedProjectDescription" Decode.string))
                )
            )
        )
