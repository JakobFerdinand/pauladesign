module Api.Project exposing (Project, allProjects)

import DataSource exposing (DataSource)
import DataSource.Http
import OptimizedDecoder as Decode
import Pages.Secrets as Secrets


type alias Project =
    { title : String
    , imageUrl : String
    , slug : String
    , description : Maybe String
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
