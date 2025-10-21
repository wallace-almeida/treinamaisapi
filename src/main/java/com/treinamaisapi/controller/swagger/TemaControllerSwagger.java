package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.TemaRequest;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@Tag(name = "Tema", description = "Usuarios do sistema")
public interface TemaControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<TemaResponse> criar(@RequestBody TemaRequest request);

    @GetMapping
    ResponseEntity<List<TemaResponse>> listar();
}
