package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.concurso.request.ConcursoRequest;
import com.treinamaisapi.common.dto.concurso.response.ConcursoResponse;
import com.treinamaisapi.common.dto.questao.request.CapituloRequest;
import com.treinamaisapi.common.dto.questao.response.CapituloResponse;
import com.treinamaisapi.entity.Concurso;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@Tag(name = "Concurso", description = "Cadastro dos Concursos")
public interface ConcursoControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<ConcursoResponse> criarConcurso(@RequestBody ConcursoRequest request);

    @GetMapping("/{id}")
    ResponseEntity<ConcursoResponse> buscarPorId(@PathVariable Long id);
}
