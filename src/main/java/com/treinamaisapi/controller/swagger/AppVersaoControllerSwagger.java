package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.versaoApp.AppVersionDTO;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Tag(name = "App", description = "Atualizacao da versao do app")
public interface AppVersaoControllerSwagger {


    @GetMapping("/version")
    AppVersionDTO getVersion();
}
