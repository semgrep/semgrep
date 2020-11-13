package servlets;

import java.io.File;
import java.io.FileInputStream;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.biz.org.Status;

import org.apache.commons.io.FilenameUtils;

@Path("/")
public class Cls
{
	// ruleid:jax-rs-path-traversal
	@GET
    @Path("/images/{image}")
    @Produces("images/*")
    public Response getImage(@javax.ws.rs.PathParam("image") String image) {
        File file = new File("resources/images/", image); //Weak point

        if (!file.exists()) {
            return Response.status(Status.NOT_FOUND).build();
        }

        return Response.ok().entity(new FileInputStream(file)).build();
    }
    
    // ok
    @GET
    @Path("/images/{image}")
    @Produces("images/*")
    public Response ok(@javax.ws.rs.PathParam("image") String image) {
        
        File file = new File("resources/images/", FilenameUtils.getName(image)); //Fix

        if (!file.exists()) {
            return Response.status(Status.NOT_FOUND).build();
        }

        return Response.ok().entity(new FileInputStream(file)).build();
    }
}