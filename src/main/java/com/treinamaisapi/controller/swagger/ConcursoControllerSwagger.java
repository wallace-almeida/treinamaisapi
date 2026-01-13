package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.concurso.request.ConcursoRequest;
import com.treinamaisapi.common.dto.concurso.response.ConcursoResponse;
import com.treinamaisapi.common.dto.concurso.response.ConcursoStatusResponse;
import com.treinamaisapi.common.dto.questao.request.CapituloRequest;
import com.treinamaisapi.common.dto.questao.response.CapituloResponse;
import com.treinamaisapi.entity.Concurso;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Concurso", description = "Cadastro dos Concursos")
public interface ConcursoControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<ConcursoResponse> criarConcurso(@RequestBody ConcursoRequest request);

    @GetMapping("/{id}")
    ResponseEntity<ConcursoResponse> buscarPorId(@PathVariable Long id);


    @GetMapping
    ResponseEntity<List<ConcursoResponse>> listarAtivos();


    @PatchMapping("/{concursoId}/ativar")
    ConcursoStatusResponse ativar(@PathVariable Long concursoId);

    @PatchMapping("/{concursoId}/encerrar")
    ConcursoResponse encerrar(@PathVariable Long concursoId);
}
