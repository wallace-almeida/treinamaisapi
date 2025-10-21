package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.CapituloRequest;
import com.treinamaisapi.common.dto.questao.response.CapituloResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@Tag(name = "Capitulo", description = "Usuarios do sistema")
public interface CapituloControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<CapituloResponse> criar(@RequestBody CapituloRequest request);

    @GetMapping("/tema/{temaId}")
    ResponseEntity<List<CapituloResponse>> listarPorTema(@PathVariable Long temaId);
}
