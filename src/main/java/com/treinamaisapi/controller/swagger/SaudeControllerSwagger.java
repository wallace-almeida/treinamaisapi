package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Tag(name = "Saude", description = "Saude do sistema")
public interface SaudeControllerSwagger {


    @GetMapping
    String health();
}
